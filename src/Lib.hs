{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Lib
    ( someFunc, hello
    ) where

import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults

connectInfo :: ConnectInfo
connectInfo = ConnectInfo "192.168.1.7" 3306 "root" "root" "reddit_bot" [] "" Nothing

--This type reflects the database table structure
data Subreddit = Subreddit{id :: Int, name :: String, daily_quota :: Int, priority :: Int, enabled :: Bool, recent_feeds_window :: Int, moderator :: Bool}

-- I have to fix this:
instance QueryResults Subreddit where
    convertResults [fa,fb] [va,vb] = User $ a * b
        where !a = convert fa va --Illegal bang-pattern (use BangPatterns)
              !b = convert fb vb --Illegal bang-pattern (use BangPatterns)
    convertResults fs vs  = convertError fs vs 2



hello :: IO Subreddit
hello = do
  conn <- connect connectInfo
  [Only i] <- query_ conn "select * from subreddits" 
  return i

someFunc :: IO ()
someFunc = Prelude.putStrLn "someFunc"
