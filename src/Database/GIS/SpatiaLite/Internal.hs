{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | SpatiaLite GIS implementation module.
-- Copyright (C) Simon Beaumont 2019 See: LICENSE for terms and conidtiions of use.
-- N.B. Still not convinced that the initialisation of the Spatialite module and schema is
-- robust.
-- Experimental

module Database.GIS.SpatiaLite.Internal 
  ( GIS
  , MonadGIS
  , runGIS
  , notGIS -- for testing only: don't do the module loading and ensure Spatialite schema  
  , queryGIS
  , queryGIS_
  , executeGIS
  , executeGIS_
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Reader

import qualified Data.Text as T
import qualified Data.Text.Encoding as E

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
data GISEnv = Env { envDB :: Connection }

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

-- open connection but no Spatialite init 
notGIS :: String -> GIS a -> IO a
notGIS uri (GIS g) = SS.open uri >>= \c -> runReaderT g (Env $ Connection c)   

-- helper to runReaderT with environment
--
runGISInternal :: Connection -> GIS a -> IO a
runGISInternal conn (GIS gis) = runReaderT gis (Env conn)

----------------------------------
--- query and execute variants ---
---------------------------------- 

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

-- | GIS Execute with no parameters
executeGIS_ :: (MonadGIS m) => SS.Query -> m ()
executeGIS_ q = liftGIS $ GIS $ do
    Connection c <- asks envDB
    liftIO $ SS.execute_ c q

-- | GIS Execute query binding params
executeGIS :: (SS.ToRow p, MonadGIS m) => SS.Query -> p -> m ()
executeGIS q p = liftGIS $ GIS $ do
  Connection c <- asks envDB
  liftIO $ SS.execute c q p

-- TODO
-- - with prepared statement etc...

--------------------------------------------------------
-- Low level calls we use to init the SpatiaLite schema
--------------------------------------------------------

-- This should work with vanilla sqlite-direct API rather than
-- our customised version which exposes the c function.
-- TODO: try to run this in a transaction along with all
-- the rest of the initialisation code to prevent a race condition
-- where the RTS is multi-threaded.
{-
loadExtension :: SS.Connection -> T.Text -> IO ()
loadExtension c name =
  SS.execute c "select load_extension(?)" (SS.Only name)
-}
-- call to ffi function since this is now marked unsafe will
-- block all threads in RTS until extension library is loaded
-- avoiding race conditions.  


loadExtension' :: SS.Connection -> T.Text -> IO ()
loadExtension' c name = 
  SD.loadExtension (SS.connectionHandle c) (toUtf8 name) >> return ()

-- Try very hard to load a mod_spatialite extension library
-- N.B. on some platforms or versions of SQLite the database function 
-- implementing this seems to do the work of trying the dylib variants 
--
loadSpatialExtension :: SS.Connection -> IO ()
loadSpatialExtension c =
  loadExtension' c "mod_spatialite" <|>
  loadExtension' c "mod_spatialite.so" <|>
  loadExtension' c "mod_spatialite.dylib"

-- Check if a table exist in the schema.

tableExists :: SS.Connection -> T.Text -> IO Bool
tableExists c t = 
  SS.query c "SELECT count(*) FROM sqlite_master where name=? and type='table'" [t] >>= \case
    ((SS.Only r):_) -> return $ (r :: Integer) > 0
    _ -> return False

-- Predicate to determine if the SpatiaLite schema exists. 
-- Not entirely robust as we could drop any number of tables and still
-- have this one - but it is probably the most crucial.

hasSpatialSchema :: SS.Connection -> IO Bool
hasSpatialSchema c = tableExists c "spatial_ref_sys"

-- This may take a while to run on slower platforms as it is very
-- IO bound...

ensureSpatialSchema :: SS.Connection -> IO ()
ensureSpatialSchema c = SS.execute_ c "SELECT InitSpatialMetaData()"

{-# INLINE toUtf8 #-}
toUtf8 :: T.Text -> SD.Utf8
toUtf8 = SD.Utf8 . E.encodeUtf8

{-
utf8toText :: SD.Utf8 -> T.Text
utf8toText (SD.Utf8 b) = E.decodeUtf8 b 

utf8toString :: SD.Utf8 -> String
utf8toString = T.unpack . utf8toText
-}
