{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | GIS Schema module allows abstraction of common GIS operations
-- well one day maybe... rigth now its SpatiaLite all the way down.
module Database.GIS.Schema 
    ( addWGS84PointColToTable
    , hasSpatiaLiteSchema
    -- 
    , module Database.SQLite.Simple.ToField
    , module Database.SQLite.Simple.FromField
    , module Database.SQLite.Simple.FromRow
    , module Database.SQLite.Simple.ToRow
    , module Database.SQLite.Simple 
    ) where

import Database.GIS.SpatiaLite

import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple 

-- | Add WGS-84 POINT column to a table
--addPointColToTable :: T.Text -> T.Text -> IO ()
addWGS84PointColToTable :: (ToField a, ToField b, MonadGIS m) => a -> b -> m ()
addWGS84PointColToTable table col = 
    executeGIS "select AddGeometryColumn(?, ?, 4326,'POINT','XY')" (table, col)
    
-- | Is the GIS a SpatiaLite schema
hasSpatiaLiteSchema :: (MonadGIS m) => m Bool
hasSpatiaLiteSchema =
    queryGIS_ "SELECT count(*) FROM sqlite_master where name='spatial_ref_sys' and type='table'" >>= \case 
      ((Only r):_) -> return $ (r :: Integer) > 0
      _ -> return False
    
