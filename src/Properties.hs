{-# LANGUAGE OverloadedStrings #-}

module Properties(
  Login(..),
  getProperty,
  getPassword,
  readLogin

) where 

import Data.Ini
import Data.Text
import Data.Text.Read
import System.IO.Unsafe

readProperties :: IO Data.Ini.Ini
readProperties = do
  a <- readIniFile "properties.txt"
  let b = either undefined id a
  return b

getProperty :: String -> String -> Ini -> Either String Text
getProperty propertyName section ini = do 
  a <- Data.Ini.lookupValue (pack propertyName) (pack section) ini
  return a


getPassword :: Either String Text
getPassword = do
  a <- getProperty "REDDIT" "password" (unsafePerformIO readProperties)
  return a

data Login = Login {
   username :: Text,
   password :: Text
   } deriving Show

readLogin :: IO (Either String Login)
readLogin = do
   eini <- readIniFile "properties.txt"
   return $ do 
      ini <- eini
      p <- lookupValue "REDDIT" "password" ini
      u <- lookupValue "REDDIT" "user" ini
      return $ Login u p

