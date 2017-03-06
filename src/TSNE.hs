module TSNE (
        prepareTSNE
    ) where

import Data.Default (def)
import Data.IORef
import Control.Concurrent
import Control.Concurrent.MVar
import Graphics.UI.GLUT (postRedisplay)
import Control.Monad (when, forever)
import Data.Maybe (isJust)
import Pipes(runEffect, for, lift)

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
    cst <- readIORef st
    when (runningTSNE cst) $ do
        mps <- tryTakeMVar v
        case mps of
            Just ps -> do
                atomicModifyIORef st (\cst -> (cst { dataPointPositions = ps }, ()))
                postRedisplay Nothing
            Nothing -> return ()

runTSNE :: MVar [Vec3] -> [DataPoint] -> IO ()
runTSNE v dps = forTsne3D (consumeTSNEOutput v) def $ map dpData dps

--runTSNE v dps = do
--    runEffect $ for (tsne3D def (map dpData dps)) $ \r -> do
--        lift $ consumeTSNEOutput v r

consumeTSNEOutput :: MVar [Vec3] -> TSNEOutput3D -> IO ()
consumeTSNEOutput v t = do
    let s = ((map toVec3).normalize.tsneSolution3D) t
    putMVar v s

normalize :: (Floating a, Ord a) => [(a,a,a)] -> [(a,a,a)]
normalize vs = zip3 (n xs) (n ys) (n zs)
    where
        (xs,ys,zs) = unzip3 vs
        n l = map ((/(maximum l - minimum l)).(subtract (minimum l))) l

toVec3 :: Real a => (a,a,a) -> Vec3
toVec3 (x,y,z) = (realToFrac x, realToFrac y, realToFrac z)

