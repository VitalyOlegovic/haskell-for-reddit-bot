{-# LANGUAGE OverloadedStrings #-}

module RedditClient(
  Link(..),
  clientCall,
  loginAndSend
) where

import Properties

import Reddit
import Reddit.Actions.Post
import Data.Text
import qualified Data.ByteString as B
import Data.String

data Link = Link{
  subreddit :: Text,
  title :: Text,
  url :: Text
} deriving Show

sendLink :: (Monad m) => Link -> RedditT m PostID
sendLink link = submitLink (R $ (subreddit link))
                      (title link)
                      (url link)

clientCall :: Properties.Login -> Link -> IO (Either (APIError RedditError) PostID)
clientCall login link = runRedditWith opt (sendLink link)
    where
          opt = defaultRedditOptions
                    { loginMethod = Credentials (username login) (password login),
                      customUserAgent = Just $ fromString "vitalprogr" }

loginAndSend :: Link -> IO (Either String PostID)
loginAndSend link = do
  logIn <- readLogin
  case logIn of Left err -> return $ Left err
                Right login -> do
                  result <- clientCall login link
                  case result of Left err -> return . Left $ show err
                                 Right postId -> return $ Right postId
