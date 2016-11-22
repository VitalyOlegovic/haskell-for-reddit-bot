{-# LANGUAGE OverloadedStrings #-}

module Properties(
  Login(..),
  Datasource(..),
  readLogin,
  readDatasource
) where 

import Data.Ini
import Data.Text
import Data.Text.Read
import Database.MySQL.Simple

data Login = Login {
   username :: Text,
   password :: Text
} deriving Show

data Datasource = Datasource {
  dbHostname :: Text,
  dbPort :: Text,
  dbUser :: Text,
  dbPassword :: Text,
  dbDatabase :: Text
} deriving Show

readLogin :: IO (Either String Login)
readLogin = do
   eini <- readIniFile "properties.txt"
   return $ do 
      ini <- eini
      redditPassword <- lookupValue "REDDIT" "password" ini
      redditUser <- lookupValue "REDDIT" "user" ini
      return $ Login redditUser redditPassword

readDatasource :: IO (Either String Properties.Datasource)
readDatasource = do
   eini <- readIniFile "properties.txt"
   return $ do
      ini <- eini
      hn <- lookupValue "DATABASE" "hostname" ini
      pr <- lookupValue "DATABASE" "port" ini
      us <- lookupValue "DATABASE" "user" ini
      pw <- lookupValue "DATABASE" "password" ini
      db <- lookupValue "DATABASE" "database" ini
      return $ Datasource hn pr us pw db
