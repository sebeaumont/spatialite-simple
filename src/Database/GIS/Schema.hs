{-# LANGUAGE OverloadedStrings #-}
module Database.GIS.Schema 
    ( addWGS84PointColToTable
    ) where

import Database.GIS.SpatiaLite

-- | Add WGS-84 POINT column to a table
--addPointColToTable :: T.Text -> T.Text -> IO ()
addWGS84PointColToTable :: (ToField a, ToField b, MonadGIS m) => a -> b -> m ()
addWGS84PointColToTable table col = 
    executeGIS "select AddGeometryColumn(?, ?, 4326,'POINT','XY')" (table, col)
    
