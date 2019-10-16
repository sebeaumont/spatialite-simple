--{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE FunctionalDependencies #-}
--{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

--import Control.Monad
import Control.Monad.IO.Class
import Database.GIS.SpatiaLite
import Database.SQLite.Simple
import qualified Data.Text as T

main :: IO ()
main = do
  putStrLn "Hello Earth..."
  foo <- runGIS "test.db" $ queryGIS_ "select count(*) from  events" 
  putStrLn (show (foo :: [Only Integer]) ++ " events are recorded in the captain's log")
  gis

gis :: IO ()
gis = runGIS "test.db" $ do
  foo <- queryGIS "select datetime(timestamp), lat, lon, x, y, z from events where x > ?" (Only (10 :: Double)) 
    :: GIS [(T.Text, Double, Double, Double, Double, Double)] 
  liftIO $ print foo
