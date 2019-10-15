{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The glorious SpatiaLite GIS implementation module.
-- Copyright (C) Simon Beaumont 2019 See: LICENSE for terms and conidtiions of use.
-- 
module Database.GIS.SpatiaLite.Internal 
  ( GIS
  , MonadGIS
  , runGIS
  , queryGIS
  , queryGIS_
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Reader

import qualified Data.Text as T

import qualified Database.SQLite.Simple as SS
import qualified Database.SQLite3.Direct as SD

-- Wrap underlying database connection
newtype Connection = Connection SS.Connection

-- |  Cautiously init the SQLite instance to make it SpatiaLite GIS
initConnection :: String -> IO Connection
initConnection uri =
  bracketOnError
    (SS.open uri)
    SS.close
    -- iff any of this fails we close the underlying connection 
    (\c -> SD.setLoadExtensionEnabled (SS.connectionHandle c) True >>
           loadSpatialExtension c >>
           SD.setLoadExtensionEnabled (SS.connectionHandle c) False >>
           (hasSpatialSchema c >>= (\ss -> unless ss (ensureSpatialSchema c))) >>
           (return . Connection) c)

-- | Close the Connection to the GIS
closeConnection :: Connection -> IO ()
closeConnection (Connection d) = SS.close d

-- | may want more here in due course...
data GISEnv = Env { envDB :: !Connection }

-- | Monad transformer stack threading access to GISEnv via ReaderT
newtype GIS a = GIS (ReaderT GISEnv IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader GISEnv)

class (Monad m) => MonadGIS m where
  liftGIS :: GIS a -> m a
  
instance MonadGIS GIS where
  liftGIS = id

-- | Run GIS actions in the context of a given database connection
runGIS :: String -> GIS a -> IO a
runGIS uri g = 
  bracket
  (liftIO $ initConnection uri)
  (liftIO . closeConnection)
  (`runGISInternal` g)

-- | Internal version without connection management
--
runGISInternal :: Connection -> GIS a -> IO a
runGISInternal conn (GIS gis) = runReaderT gis (Env conn)
 
--- | GIS Query 
queryGIS :: (SS.ToRow p, SS.FromRow a, MonadGIS m) => SS.Query -> p -> m [a]
queryGIS q p = liftGIS $ GIS $ do
    Connection c <- asks envDB
    liftIO $ query c q p -- this type ambigous due to row length uncertainty!
  where
    query :: (SS.ToRow a, SS.FromRow r) => SS.Connection -> SS.Query -> a -> IO [r]
    query = SS.query

-- | GIS Query with no parameters 
queryGIS_ :: (SS.FromRow a, MonadGIS m) => SS.Query -> m [a]
queryGIS_ q  = liftGIS $ GIS $ do
    Connection c <- asks envDB
    liftIO $ query c q  -- this type ambigous due to row length uncertainty!
  where
    query :: (SS.FromRow r) => SS.Connection -> SS.Query -> IO [r]
    query = SS.query_    


--------------------------------------------------------
-- Low level calls we use to init the SpatiaLite schema
--------------------------------------------------------

-- This should work with vanilla sqlite-direct API rather than
-- our customised version which exposes the c function.

loadExtension :: SS.Connection -> T.Text -> IO ()
loadExtension c name = SS.execute c "select load_extension(?)" (SS.Only name)

-- Try very hard to load a mod_spatialite extension library
-- N.B. on some platforms or versions of SQLite the database function 
-- implementing this seems to do this work for us... 

loadSpatialExtension :: SS.Connection -> IO ()
loadSpatialExtension c =
  loadExtension c "mod_spatialite" <|> 
  loadExtension c "mod_spatialite.so" <|>
  loadExtension c "mod_spatialite.dylib"

-- Check if a table exist in the schema.

tableExists :: SS.Connection -> T.Text -> IO Bool
tableExists c t = 
  SS.query c "SELECT count(*) FROM sqlite_master where name=? and type='table'" [t] >>= \case
    ((SS.Only r):_) -> return $ (r :: Integer) < 1
    _ -> return False

-- Predicate to determine if the SpatiaLite schema exists. 
-- Not entirely robust as we could drop any number of tables and still
-- have this one - but it is probably the most crucial. YMMV

hasSpatialSchema :: SS.Connection -> IO Bool
hasSpatialSchema c = tableExists c "spatial_ref_sys"

-- This may take a while to run on slower platforms as it is very
-- IO bound...

ensureSpatialSchema :: SS.Connection -> IO ()
ensureSpatialSchema c = SS.execute_ c "SELECT InitSpatialMetaData()"

    

