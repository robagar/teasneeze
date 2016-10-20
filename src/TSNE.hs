module TSNE (
        prepareTSNE
    ) where

import Data.IORef
import Control.Concurrent
import Control.Concurrent.MVar
import Graphics.UI.GLUT (postRedisplay)
import Control.Monad (when, forever)
import Data.Maybe (isJust)

import Data.Algorithm.TSNE

import Util
import State
import DataPoints


prepareTSNE :: IORef AppState -> [DataPoint] -> IO (IO ())
prepareTSNE st dps = do
    v <- newEmptyMVar
    forkIO $ runTSNE v dps
    return $ renderOnTSNE st v

renderOnTSNE :: IORef AppState -> MVar [Vec3] -> IO ()
renderOnTSNE st v = do
    mps <- tryTakeMVar v
    case mps of
        Just ps -> do
            atomicModifyIORef st (\cst -> (cst { dataPointPositions = ps }, ()))
            postRedisplay Nothing
        Nothing -> return ()

runTSNE :: MVar [Vec3] -> [DataPoint] -> IO ()
runTSNE v dps = do 
    pss <- tsne 30 10 (map dpData dps) 
    forever $ do
        putMVar v $ (normalize.head) pss

normalize :: [Vec3] -> [Vec3]
normalize vs = zip3 (n xs) (n ys) (n zs)
    where
        (xs,ys,zs) = unzip3 vs
        n l = map ((/(maximum l - minimum l)).(subtract (minimum l))) l

