{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}

module Persistence(
  getSubreddits, getFeedIdsBySubreddit, getFeedById,
  Subreddit(..), Feed(..), FeedSubreddit(..)
) where

import Properties

import Data.Text as T
import Data.ByteString.Char8 as BS
import GHC.Word
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result

{-
  See https://hackage.haskell.org/package/mysql-simple-0.4.0.1/docs/Database-MySQL-Simple.html
-}

getConnection :: IO (Either String (IO Connection))
getConnection = do
  ds <- readDatasource
  case ds of Left err -> return (Left $ show err)
             Right dataSource -> return $ Right (connect (dataSourceToConnectInfo dataSource))

dataSourceToConnectInfo :: Properties.Datasource -> ConnectInfo
dataSourceToConnectInfo datasource =   ConnectInfo{
    connectHost = T.unpack $ dbHostname datasource
  , connectPort = read (T.unpack $ dbPort datasource) :: GHC.Word.Word16
  , connectUser = T.unpack $ dbUser datasource
  , connectPassword = T.unpack $ dbPassword datasource
  , connectDatabase = T.unpack $ dbDatabase datasource
  , connectOptions = []
  , connectPath = ""
  , connectSSL = Nothing
}

data Subreddit = Subreddit{
    subredditId :: Integer
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

data Feed = Feed{
    feedId :: Integer
  , url :: String
  , parent_feed :: Integer
} deriving Show

instance QueryResults Feed where
  convertResults [fieldId,fieldUrl,fieldParentFeed] [valueId,valueUrl,valueParentFeed]
    = Feed x1 x2 x3
    where
    !x1 = convert fieldId valueId
    !x2 = convert fieldUrl valueUrl
    !x3 = convert fieldParentFeed valueParentFeed

    convertResults fs vs  = convertError fs vs 2

data FeedSubreddit = FeedSubreddit{
    fsFeedId :: Integer
  , fsSubredditId :: Integer
  , flair :: Maybe String
} deriving Show

instance QueryResults FeedSubreddit where
  convertResults [fieldFeedId, fieldSubredditId, fieldFlair] [valueFeedId, valueSubredditId, valueFlair]
    = FeedSubreddit x1 x2 x3
    where
      !x1 = convert fieldFeedId valueFeedId
      !x2 = convert fieldSubredditId valueSubredditId
      !x3 = convert fieldFlair valueFlair

      convertResults fs vs  = convertError fs vs 2

getSubreddits :: IO (Either String [Subreddit])
getSubreddits = do
  conn <- getConnection
  case conn of Left err -> return $ ( Left $ show err )
               Right c -> do
                 connection <- c
                 r <- query_ connection "select * from subreddits where enabled = true order by priority"
                 mapM_ (Prelude.putStrLn . show) r
                 return $ Right r

getFeedIdsBySubreddit :: Subreddit -> IO (Either String [FeedSubreddit])
getFeedIdsBySubreddit subreddit  = do
  conn <- getConnection
  case conn of Left err -> return $ ( Left $ show err )
               Right c -> do
                 connection <- c
                 let subId = subredditId subreddit
                 r <- query connection "select * from feed_subreddit where subreddit_id = ?" [subId :: Integer]
                 mapM_ (Prelude.putStrLn . show) r
                 return $ Right r

getFeedById :: Integer -> IO (Either String [Feed])
getFeedById identifier = do
  conn <- getConnection
  case conn of Left err -> return $ ( Left $ show err )
               Right c -> do
                 connection <- c
                 r <- query connection "select * from feeds where id = ?" [identifier :: Integer]
                 mapM_ (Prelude.putStrLn . show) r
                 return $ Right r
