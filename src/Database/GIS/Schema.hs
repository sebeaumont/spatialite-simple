{-# LANGUAGE OverloadedStrings #-}
module Database.GIS.Schema 
    ( addWGS84PointColToTable
    ) where

import Database.GIS.SpatiaLite.Internal
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromRow

-- | Add WGS-84 POINT column to a table
--addPointColToTable :: T.Text -> T.Text -> IO ()
addWGS84PointColToTable :: (ToField a, ToField b, FromRow r, MonadGIS m) => a -> b -> m [r]
addWGS84PointColToTable table col = 
    queryGIS "select AddGeometryColumn(?, ?, 4326,'POINT','XY')" (table, col)
    
