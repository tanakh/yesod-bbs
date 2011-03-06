{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import Control.Applicative
import Data.Maybe
import Data.Time

import YesodBBS

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- YesodBBS.hs; look for the line beginning with mkYesodData.
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
  posts <- runDB $ selectList [] [PostDateDesc] 10 0
  
  defaultLayout $ do
    h2id <- lift newIdent
    setTitle "Yesod BBS"
    addWidget $(widgetFile "homepage")

postRootR :: Handler ()
postRootR = do
  (mname, mmail, content) <- runFormPost' $ (,,)
    <$> maybeStringInput "name"
    <*> maybeStringInput "mail"
    <*> stringInput "content"

  cur <- liftIO $ getCurrentTime

  runDB $
    insert $ Post (fromMaybe "名無しさん" mname) (fromMaybe "" mmail) content cur

  redirect RedirectTemporary RootR
