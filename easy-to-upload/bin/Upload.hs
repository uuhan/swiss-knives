{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Upload where

import           Yesod
import           Yesod.Core.Types
import           Yesod.Form

import           Data.Text(unpack)
import           Network.HTTP.Types
import           System.FilePath((</>))

data App = App 
    { dir :: FilePath }

mkYesod "App" [parseRoutes|
/   HomeR   GET POST            
|]

instance Yesod App where
    makeSessionBackend _ = pure Nothing
    maximumContentLength _ (Just HomeR) = Just $ 2 * 1024 * 1024 * 1024
    maximumContentLength _ _ = Just $ 2 * 1024 * 1024

getHomeR :: Handler Html
getHomeR = do
    (w, enctype) <- generateFormPost upload
    defaultLayout $ do
        [whamlet|
        <div>
            <form method="post" action="@{HomeR}" enctype=#{enctype}>
                ^{w}
                <input type="submit" value="提交">
        |]

postHomeR :: Handler ()
postHomeR = do
    ((res, w), _) <- runFormPost upload
    App{..} <- getYesod
    case res of
        FormSuccess FileInfo{..} -> do
            $(logInfo) "Moving..."
            lift $ fileMove $ dir </> (unpack fileName)
            redirect HomeR
        _ -> do
            $(logError) "Form Error"
            redirect HomeR

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage 

upload :: Form FileInfo
upload = renderDivs $ areq fileField "文件" Nothing
