{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module LoadData where

import GHC.Generics
import System.FilePath
import qualified Data.ByteString.Lazy as BL
import Data.Aeson

import DataPoints (DataPoint (..))


loadData :: FilePath -> IO (Either String [DataPoint])
loadData f = do 
    erd <- loadRawData f
    return $ rawDataToDataPoints (takeDirectory f) <$> erd

data RawDataResult = RawDataResult {
        classification :: String,
        image_path :: String,
        x :: Float,
        y :: Float,
        z :: Float
    } deriving (Generic, FromJSON)

data RawData = RawData {
        name :: String,
        results :: [RawDataResult]
    } deriving (Generic, FromJSON)

loadRawData :: FilePath -> IO (Either String RawData) 
loadRawData f = do
    rd <- BL.readFile f
    return $ eitherDecode rd

rawDataToDataPoints :: FilePath -> RawData -> [DataPoint]
rawDataToDataPoints dir rd = map dp (results rd)
    where dp r = DataPoint (x r, y r, z r) (dir </> image_path r)