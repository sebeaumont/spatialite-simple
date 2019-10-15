{-# LANGUAGE OverloadedStrings #-}
module Database.GIS.Schema 
    ( addPointColToTable
    ) where

import Database.GIS.SpatiaLite.Internal
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromRow

-- | Add WGS-84 POINT column to a table
--addPointColToTable :: T.Text -> T.Text -> IO ()
addPointColToTable :: (ToField a, ToField b, FromRow r, MonadGIS m) => a -> b -> m [r]
addPointColToTable table col = 
    gisQuery "select AddGeometryColumn(?, ?, 4326,'POINT','XY')" (table, col)
    
