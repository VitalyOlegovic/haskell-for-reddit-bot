{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}

module Persistence(
  someFunc,
  getSubredditList
) where

import Properties

import Data.Text
import GHC.Word
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result

getDatasource :: IO (Either String (IO Connection))
getDatasource = do
  ds <- readDatasource
  case ds of Left error -> return (Left $ show error)
             Right dataSource -> return $ Right (connect (dataSourceToConnectInfo dataSource))

dataSourceToConnectInfo :: Properties.Datasource -> ConnectInfo
dataSourceToConnectInfo datasource =   ConnectInfo{
    connectHost = unpack $ dbHostname datasource
  , connectPort = read (unpack $ dbPort datasource) :: GHC.Word.Word16
  , connectUser = unpack $ dbUser datasource
  , connectPassword = unpack $ dbPassword datasource
  , connectDatabase = unpack $ dbDatabase datasource
  , connectOptions = []
  , connectPath = ""
  , connectSSL = Nothing
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
    
getSubredditList :: IO (Either String [Subreddit])
getSubredditList = do
  conn <- getDatasource
  case conn of Left error -> return $ ( Left $ show error )
               Right c -> do
                 connection <- c
                 r <- query_ connection "select * from subreddits" 
                 mapM_ (Prelude.putStrLn . show) r
                 return $ Right r


someFunc :: IO ()
someFunc = Prelude.putStrLn "someFunc"


