{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module LoadData where

import GHC.Generics
import System.FilePath
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Data.Maybe

import DataPoints (
        DataSet (..),
        DataPoint (..)
    )
import Util


loadDataSet :: FilePath -> IO (Either String DataSet)
loadDataSet f = do 
    dsi <- loadDataSetInfo f
    return $ makeDataSet (takeDirectory f) <$> dsi

data DataPointInfo = DataPointInfo {
        classification :: Maybe String,
        image_path :: String,
        x :: Float,
        y :: Float,
        z :: Float
    } deriving (Generic, FromJSON)

data DataSetInfo = DataSetInfo {
        name :: Maybe String,
        data_points :: [DataPointInfo]
    } deriving (Generic, FromJSON)

loadDataSetInfo :: FilePath -> IO (Either String DataSetInfo) 
loadDataSetInfo f = do
    s <- BL.readFile f
    return $ eitherDecode s

makeDataSet :: FilePath -> DataSetInfo -> DataSet
makeDataSet dir dsi = DataSet n (map dp dps) (Just (map p dps))
    where
        n = fromMaybe "(unnamed)" (name dsi)
        dps = data_points dsi 
        dp r = DataPoint (dir </> image_path r)
        p r = Vec3 (x r, y r, z r)

