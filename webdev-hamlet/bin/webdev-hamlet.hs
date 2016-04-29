{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import           Yesod
import           Text.Hamlet.Runtime as R
import           System.Environment
import           System.IO

data App = App 
    { tpl :: FilePath }

mkYesod "App" [parseRoutes|
/       HomeR       GET            
|]

instance Yesod App where
    makeSessionBackend _ = pure Nothing

getHomeR :: Handler Html
getHomeR = do
    App{..} <- getYesod
    tmpl <- R.readHamletTemplateFile defaultHamletSettings tpl
    site <- R.renderHamletTemplate tmpl []
    defaultLayout $ do
        toWidget $ \render -> do
            site

main :: IO ()
main = do 
    getArgs >>=
        \case
            (tpl:_) -> do
                (lookup "PORT" <$> getEnvironment)
                    >>= 
                        maybe
                            (warp 3000 App{..})
                            (const $ warpEnv App{..})
            _ -> do
                hPutStrLn stderr "Please Specify A Hamlet File"
