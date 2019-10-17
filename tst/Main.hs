{-# LANGUAGE OverloadedStrings #-}
-- | Testing...
module Main where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import Database.GIS.SpatiaLite
import Database.GIS.Schema

import qualified Data.Text as T
--import qualified Data.ByteString as B

main :: IO ()
main = gis
 
-- custom row type for result
data Place = Place Integer T.Text Double Double T.Text deriving (Show)
instance FromRow Place where
  fromRow = Place <$> field <*> field <*> field <*> field <*> field

-- | Run some rudimentary tests on the db
gis :: IO ()
gis = runGIS "test.db" $ do
  -- check if this is spatialite
  liftIO $ putStr "Check this is a spatialite instance: "
  spatialite <- hasSpatiaLiteSchema 
  liftIO $ print spatialite

  -- if we have a schema we assume the module is loaded... in fact that is a check on proper init of GIS
  -- so we go ahead and do some stuff 
  when spatialite $ do
    -- pleaty of side effects TODO monadic DSL anyone?
    executeGIS_ "CREATE table foobar (id integer primary key autoincrement, name text, lat float, lon float)"
    executeGIS "INSERT into foobar values (null, ?, ?, ?)" ("someplace" :: T.Text, 50.1234 :: Double, -1.0234 :: Double)
    addWGS84PointColToTable ("foobar" :: T.Text) ("position" :: T.Text)
    executeGIS_ "UPDATE foobar SET position = MakePoint(lon, lat, 4326)"
    foobar <- queryGIS_ "SELECT id, name, lat, lon, AsText(position) from foobar" 
    --mapM_ (liftIO . print) (foobar :: [(Integer, T.Text, Double, Double, T.Text)]) 
    mapM_ (liftIO . print) (foobar :: [Place]) 
