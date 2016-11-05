{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}

module Persistence
    ( someFunc, getSubredditList
    ) where

import Properties

import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result

connectInfo :: ConnectInfo
connectInfo = ConnectInfo{
    connectHost="192.168.1.7" 
  , connectPort=3306
  , connectUser="root"
  , connectPassword="root"
  , connectDatabase="reddit_bot"
  , connectOptions=[]
  , connectPath=""
  , connectSSL=Nothing
}


data Subreddit = Subreddit{
    id :: Integer
  , name :: String
  , daily_quota :: Integer
  , priority :: Integer
  , enabled :: Bool
  , recent_feeds_window :: Maybe Integer
  , moderator :: Bool
  } deriving Show

instance QueryResults Subreddit where
    convertResults [f1,f2,f3,f4,f5,f6,f7] [v1,v2,v3,v4,v5,v6,v7] 
      = Subreddit x1 x2 x3 x4 x5 x6 x7
     where 
       !x1 = convert f1 v1 
       !x2 = convert f2 v2
       !x3 = convert f3 v3
       !x4 = convert f4 v4
       !x5 = convert f5 v5
       !x6 = convert f6 v6
       !x7 = convert f7 v7
              
    convertResults fs vs  = convertError fs vs 2
    
getSubredditList :: IO [Subreddit]
getSubredditList = do
  conn <- connect connectInfo
  r <- query_ conn "select * from subreddits" 
  mapM_ (Prelude.putStrLn . show) r
  return r


someFunc :: IO ()
someFunc = Prelude.putStrLn "someFunc"


