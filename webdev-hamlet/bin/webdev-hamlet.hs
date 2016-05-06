{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import           Yesod
import           Yesod.Static
import           Text.Hamlet.Runtime as R
import           System.Environment
import           System.IO

data App = App
    { tpl       :: FilePath
    , getStatic :: Static
    }

mkYesod "App" [parseRoutes|
/       HomeR       GET            
/assets StaticR     Static getStatic
|]

instance Yesod App where
    makeSessionBackend _ = pure Nothing
    defaultLayout w = do
        PageContent{..} <- widgetToPageContent w
        withUrlRenderer [hamlet|
        $newline never
        $doctype 1.1
        <html>
            <head>
                ^{pageHead}
            <body>
                ^{pageBody}
        |]

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
            (dir:tpl:_) -> do
                getStatic <- static dir
                (lookup "PORT" <$> getEnvironment)
                    >>=
                        maybe
                            (warp 3000 App{..})
                            (const $ warpEnv App{..})
            (tpl:_) -> do
                getStatic <- static "."
                (lookup "PORT" <$> getEnvironment)
                    >>=
                        maybe
                            (warp 3000 App{..})
                            (const $ warpEnv App{..})
            _ -> do
                hPutStrLn stderr "Please Specify A Hamlet File"
