extern crate libc;

use libc::*;
use std::fs::metadata;
use std::thread;
use std::io::Error;
use std::process::Command;

fn safe_exit(mnt: &str) {
    if !metadata(mnt).map(|m| m.is_dir()).unwrap_or(false) {
        println!("{:>4}: 目录不存在", mnt);
        unsafe { exit(1) };
    } else {
        println!("{:>4}: 目录存在", mnt);
    }
}

fn mount_wraper(source: &str, target: &str, fstype: &str, flags: u64) {
    use std::ffi::CString;
    use std::ptr::null;

    unsafe {
        mount(CString::new(source).unwrap_or_else(|e| panic!("{:?}", e)).as_ptr(),
              CString::new(target).unwrap_or_else(|e| panic!("{:?}", e)).as_ptr(),
              CString::new(fstype).unwrap_or_else(|e| panic!("{:?}", e)).as_ptr(),
              flags,
              null());
        println!("{:>4}: {}", target, Error::last_os_error());
    }
}

fn main() {
    let dirs = vec!["proc", "sys", "dev"];
    let handlers: Vec<_> = dirs.into_iter()
                               .map(|d| {
                                   thread::spawn(move || {
                                       safe_exit(d);
                                   })
                               })
                               .collect();

    for h in handlers {
        h.join().unwrap();
    }

    let mnts: Vec<_> = vec!{
        thread::spawn(move || {
            mount_wraper("proc", "proc", "proc", 0)
        }),
        thread::spawn(move || {
            mount_wraper("sys", "sys", "sysfs", 0)
        }),
        thread::spawn(move || {
            mount_wraper("/dev", "dev", "", MS_BIND|MS_REC)
        }),
    };

    for m in mnts {
        m.join().unwrap();
    }

    unsafe {
        if chroot(".\0".as_ptr() as *const i8) == 0 {
            let pstatus = Command::new("/bin/bash")
                              .status()
                              .unwrap_or_else(|e| panic!("{:?}\n{}", e, Error::last_os_error()));
            if pstatus.success() {
                println!("退出 chroot!");
                // if umount("proc\0".as_ptr() as *const i8) != 0 {
                // println!("错误信息: {}", Error::last_os_error());
                // };
                // if umount("dev\0".as_ptr() as *const i8) != 0 {
                // println!("错误信息: {}", Error::last_os_error());
                // };
                // if umount("sys\0".as_ptr() as *const i8) != 0 {
                // println!("错误信息: {}", Error::last_os_error());
                // };
            } else {
                println!("bash 异常!");
            }
            println!("信息: {}", Error::last_os_error());
        }
    }
}
