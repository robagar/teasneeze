{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module LoadData where

import GHC.Generics
import System.FilePath
import qualified Data.ByteString.Lazy as BL
import Data.Aeson

import DataPoints (
        DataSet (..),
        DataPoint (..)
    )


loadDataSet :: FilePath -> IO (Either String DataSet)
loadDataSet f = do 
    dsi <- loadDataSetInfo f
    return $ makeDataSet (takeDirectory f) <$> dsi

data DataPointInfo = DataPointInfo {
        classification :: String,
        image_path :: String,
        x :: Float,
        y :: Float,
        z :: Float
    } deriving (Generic, FromJSON)

data DataSetInfo = DataSetInfo {
        name :: String,
        data_points :: [DataPointInfo]
    } deriving (Generic, FromJSON)

loadDataSetInfo :: FilePath -> IO (Either String DataSetInfo) 
loadDataSetInfo f = do
    s <- BL.readFile f
    return $ eitherDecode s

makeDataSet :: FilePath -> DataSetInfo -> DataSet
makeDataSet dir dsi = DataSet (name dsi) (map dp (data_points dsi))
    where dp r = DataPoint (x r, y r, z r) (dir </> image_path r)