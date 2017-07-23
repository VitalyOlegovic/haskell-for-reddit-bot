module Main where

import Data.Text
import Reddit.Types.Post

import Persistence
import Properties
import RedditClient
import Reddit

{-
main :: IO (Either String Reddit.Types.Post.PostID)
main = loginAndSend $ RedditClient.Link (pack "testitaly") (pack "prova") (pack "https://duckduckgo.com/")
-}

main :: IO()
main = readSubreddits

readSubreddits :: IO()
readSubreddits =  do
  eitherSubreddits <- getSubreddits
  case eitherSubreddits of Left err -> print err
                           Right subreddits -> mapM_ readFeedIds subreddits

readFeedIds :: Persistence.Subreddit -> IO ()
readFeedIds sub = do
  eitherFeedIds <- getFeedIdsBySubreddit sub
  case eitherFeedIds of Left err -> print err
                        Right feeds -> mapM_ readFeed feeds

readFeed :: FeedSubreddit -> IO()
readFeed feedSubreddit = do
  let identifier = fsFeedId feedSubreddit :: Integer
  eitherFeed <- getFeedById identifier
  case eitherFeed of Left err -> print err
                     Right feed -> mapM_ (putStrLn . show) feed
