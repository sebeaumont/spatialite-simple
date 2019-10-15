--{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE FunctionalDependencies #-}
--{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Database.GIS.SpatiaLite
import Database.SQLite.Simple
import qualified Data.Text as T

main :: IO ()
main = do
  putStrLn "Hello Earth..."
  foo <- runGIS "test.db" $ gisQuery_ "select count(*) from test" 
  print $ (foo :: [Only Integer])
  gis

gis :: IO ()
gis = runGIS "test.db" $ do
  foo <- (gisQuery "select * from test where id < ?" (Only (3 :: Integer))) :: GIS [(Integer, T.Text)] 
  liftIO $ print foo
  