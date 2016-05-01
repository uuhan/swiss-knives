{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Main (main) where

import           Yesod
import           Upload (App(..))

import           System.Directory
import           System.Environment
import           System.IO

main :: IO ()
main = do
    getArgs >>=
        \case 
            [] -> hPutStrLn stderr "未指定目录"
            (dir:_) -> do
                if
                    | b <- doesDirectoryExist dir -> do
                        warp 3000 $ App dir
                    | otherwise -> do
                        hPutStrLn stderr "目录不存在"

