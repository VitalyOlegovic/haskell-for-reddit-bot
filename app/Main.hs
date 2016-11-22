module Main where

import Data.Text
import Reddit.Types.Post

import Persistence
import Properties
import RedditClient
import Reddit

main :: IO (Either String Reddit.Types.Post.PostID)
main = loginAndSend $ RedditClient.Link (pack "testitaly") (pack "prova") (pack "https://duckduckgo.com/")
