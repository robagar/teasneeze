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
    ts <- tsne def (map dpData dps)
    consumeTSNEOutput v ts 

consumeTSNEOutput :: MVar [Vec3] -> [TSNEOutput3D] -> IO ()
consumeTSNEOutput v (t:ts) = do
    putStrLn $ "tSNE iteration " ++ show (tsneIteration t) ++ ", cost " ++ show (tsneCost t)
    putStrLn $ show (tsneSolution3D t !! 0)
    let s = ((map toVec3).normalize.tsneSolution3D) t
    putStrLn $ show (s !! 0)
    putMVar v s
    consumeTSNEOutput v ts
consumeTSNEOutput _ [] = return ()

normalize :: (Floating a, Ord a) => [(a,a,a)] -> [(a,a,a)]
normalize vs = zip3 (n xs) (n ys) (n zs)
    where
        (xs,ys,zs) = unzip3 vs
        n l = map ((/(maximum l - minimum l)).(subtract (minimum l))) l

toVec3 :: Real a => (a,a,a) -> Vec3
toVec3 (x,y,z) = (realToFrac x, realToFrac y, realToFrac z)

