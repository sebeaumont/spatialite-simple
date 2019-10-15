{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
--{-# LANGUAGE FunctionalDependencies #-}
--{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables  #-}
-- {-# LANGUAGE FunctionalDependencies #-}

module Database.GIS.SpatiaLite.Internal where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Reader

import qualified Data.Text as T

import qualified Database.SQLite.Simple as SS
--import qualified Database.SQLite3 as SQL
import qualified Database.SQLite3.Direct as SD

newtype Connection = Connection SS.Connection

initConnection :: String -> IO Connection
initConnection uri = do
  c <- SS.open uri
  -- TODO if any of these fail close the connection... and re-throw like bracket but only if exception
  _  <- SD.setLoadExtensionEnabled (SS.connectionHandle c) True
  _ <- loadSpatialExtension c
  _  <- SD.setLoadExtensionEnabled (SS.connectionHandle c) False
  ss <- hasSpatialSchema c
  unless ss (ensureSpatialSchema c)
  --
  return $ Connection c

closeConnection :: Connection -> IO ()
closeConnection (Connection d) = SS.close d

-- | may want more here in due course...
data GISEnv = Env { envDB :: !Connection }

newtype GIS a = GIS (ReaderT GISEnv IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader GISEnv)

class (Monad m) => MonadGIS m where
  liftGIS :: GIS a -> m a
  
instance MonadGIS GIS where
  liftGIS = id


-- | Run GIS actions in the context of a given database URI
--
runGIS :: String -> GIS a -> IO a
runGIS uri g = 
  bracket
  (liftIO $ initConnection uri)
  (liftIO . closeConnection)
  (`runGISInternal` g)

-- | Internal version without connection management
--
runGISInternal :: Connection -> GIS a -> IO a
runGISInternal conn (GIS redis) = runReaderT redis (Env conn)
 
-- TODO: nned a context to run something extracting the connection
-- and returning a result in the context...
{-
data Result where
  Cool :: SS.FromRow a => [a] -> Result
  Uncool :: String -> Result
          
class GISResult a where
    decode :: Result -> a
  
instance GISResult Result where
  decode = id

class (MonadGIS m) => GISContext m where
  returnDecode :: GISResult a => Result -> m a
  
instance GISContext GIS where
  returnDecode = return . decode

-- more type foo nonsense

gisQuery :: (SS.ToRow p, GISContext m, GISResult a) => SS.Query -> p -> m a
gisQuery q p = do
  r' <- liftGIS $ GIS $ do
    Connection c <- asks envDB
    liftIO $ query c q p -- this type ambigous due to row length uncertainty!
  returnDecode $ Cool r'
  where
    query :: (SS.ToRow a, SS.FromRow r) => SS.Connection -> SS.Query -> a -> IO [r]
    query = SS.query
-}
-- whereas these needs type at trun time -- polydoodle morphism
gisQuery :: (SS.ToRow p, SS.FromRow a, MonadGIS m) => SS.Query -> p -> m [a]
gisQuery q p = liftGIS $ GIS $ do
    Connection c <- asks envDB
    liftIO $ query c q p -- this type ambigous due to row length uncertainty!
  where
    query :: (SS.ToRow a, SS.FromRow r) => SS.Connection -> SS.Query -> a -> IO [r]
    query = SS.query

gisQuery_ :: (SS.FromRow a, MonadGIS m) => SS.Query -> m [a]
gisQuery_ q  = liftGIS $ GIS $ do
    Connection c <- asks envDB
    liftIO $ query c q  -- this type ambigous due to row length uncertainty!
  where
    query :: (SS.FromRow r) => SS.Connection -> SS.Query -> IO [r]
    query = SS.query_    

------------
-- Low level calls we use to init the SpatiaLite schema

loadExtension :: SS.Connection -> T.Text -> IO ()
loadExtension c name = SS.execute c "select load_extension(?)" (SS.Only name)

loadSpatialExtension :: SS.Connection -> IO ()
loadSpatialExtension c =
  -- neat but... might have to do this by hand... 
  loadExtension c "mod_spatialite" <|> 
  loadExtension c "mod_spatialite.so" <|>
  loadExtension c "mod_spatialite.dylib"

tableExists :: SS.Connection -> T.Text -> IO Bool
tableExists c t = 
  SS.query c "SELECT count(*) FROM sqlite_master where name=? and type='table'" [t] >>= \case
    ((SS.Only r):_) -> return $ (r :: Integer) < 1
    _ -> return False
  
hasSpatialSchema :: SS.Connection -> IO Bool
hasSpatialSchema c = tableExists c "spatial_ref_sys"

ensureSpatialSchema :: SS.Connection -> IO ()
ensureSpatialSchema c = SS.execute_ c "SELECT InitSpatialMetaData()"

    

