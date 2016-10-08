module DataPoints where

import Graphics.UI.GLUT
import qualified Graphics.Rendering.OpenGL as OpenGL
import Graphics.Rendering.OpenGL.Util
import System.Random

import Util

data DataSet = DataSet {
    dsName :: String,
    dsDataPoints :: [DataPoint]
}

data DataPoint = DataPoint {
    dpPosition :: (Float, Float, Float),
    dpImage :: String
}

prepareRenderDataPoints :: [DataPoint] -> IO [IO ()]
prepareRenderDataPoints dps = do
    --p <- require <$> loadProgram "shaders/billboard.vs" "shaders/texture.fs"
    p <- require <$> loadProgram "shaders/basic_model_view_projection.vs" "shaders/texture.fs"
    m <- datapointMesh
    mapM (prepareRenderDataPoint p m) dps

prepareRenderDataPoint :: Program -> Mesh -> DataPoint -> IO (IO ())
prepareRenderDataPoint p m dp = do 
    t <- require <$> loadPNGTexture (dpImage dp)
    return $ render t
    where
    render t = preservingMatrix $ do
        currentProgram $= Just p
        setSamplers p [ Sampler "tex" (imgObject t) ]
        translate $ Vector3 (x - 0.5) (y - 0.5) (z - 0.5)
        scale s s s
        renderMesh m
    (x, y, z) = dpPosition dp
    s = 0.05 :: GLfloat

datapointMesh :: IO Mesh
datapointMesh = createMesh vps tis (Just ts) Nothing []
    where vps = [ -0.5,  0.5, 0.0, 
                  -0.5, -0.5, 0.0, 
                   0.5, -0.5, 0.0,
                   0.5,  0.5, 0.0 ]
          tis = [ 0, 1, 2,
                  2, 3, 0 ]
          ts = [ 0, 0,
                 0, 1,
                 1, 1,
                 1, 0 ]

testDataPoints :: [DataPoint]
testDataPoints = [
                     DataPoint (0.5,0.5,0.5) "textures/test.png",
                     DataPoint (1,1,1) "textures/test.png"
                 ]

randomDataPoints :: Int -> IO [DataPoint]
randomDataPoints n = do
    g <- getStdGen
    let (g', g'') = split g
        xs = take n $ (randoms g) :: [Float]
        ys = take n $ (randoms g') :: [Float]
        zs = take n $ (randoms g'') :: [Float]
        ps = zip3 xs ys zs

    return $ map (uncurry DataPoint) $ zip ps (repeat "textures/test.png") 
        


loadDataPoints :: IO [DataPoint]
loadDataPoints = randomDataPoints 100