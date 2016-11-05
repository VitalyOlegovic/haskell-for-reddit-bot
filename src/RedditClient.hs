{-# LANGUAGE OverloadedStrings #-}

module RedditClient(
  Link(..),
  clientCall
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

sendLink :: Link -> Reddit PostID
sendLink = submitLink (R $ (subreddit link))
                      (title link)
                      (url link)

clientCall :: Properties.Login -> Link -> IO (Either (APIError RedditError) PostID)
clientCall login = runRedditWith opt sendLink
    where
          opt = defaultRedditOptions
                    { loginMethod = Credentials (username login) (password login),
                      customUserAgent = Just $ fromString "vitalprogr" }

loginAndSend link = clientCall readLogin link
