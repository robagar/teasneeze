module Main where

import System.Exit (exitSuccess)
import Control.Monad (forever, when)
import Graphics.UI.GLUT
import qualified Graphics.Rendering.OpenGL as OpenGL
import Graphics.Rendering.OpenGL.Util
import Data.IORef

import Opts
import Camera
import Spherical
import State
import DataPoints
import LoadData
import Util

main :: IO ()
main = do
    opts <- teasneezeOpts
    ds <- require <$> loadDataSet (inputDataFilePath opts)
    w <- initRenderWindow $ "Tea Sneeze - " ++ (dsName ds)
    b <- prepareRenderOutlineBox
    dprs <- prepareRenderDataPoints $ dsDataPoints ds
    st <- newIORef $ AppState 2 (pi/2) 0 1

    keyboardMouseCallback $= Just (onKeyMouse st)

    renderLoop w st ([b] ++ dprs) 

prepareRenderOutlineBox :: IO (AppState -> IO ())
prepareRenderOutlineBox = do
    p <- require <$> loadProgram "shaders/basic_model_view_projection.vs" "shaders/flat_green.fs"
    return $ render p
    where
    render p _ = preservingMatrix $ do
        currentProgram $= Just p
        renderObject Wireframe (Cube 1)     

initRenderWindow :: String -> IO Window
initRenderWindow title = do
    putStrLn title

    getArgsAndInitialize

    --dumpGLUTInfo

    initialDisplayMode $=
        [ 
            DoubleBuffered,
            WithSamplesPerPixel 4, 
            WithDepthBuffer
        ]

    initialWindowSize $= Size 800 600
    --actionOnWindowClose $= MainLoopReturns

    w <- createWindow title

    return w
    
--dumpGLUTInfo :: IO ()
--dumpGLUTInfo = do
--    v <- get glutVersion
--    putStrLn $ "GLUT version: " ++ show v

quitOnEsc :: KeyboardCallback
quitOnEsc c _ = when (c == '\ESC') exitSuccess

onKeyMouse :: IORef AppState -> KeyboardMouseCallback
onKeyMouse _ (Char c) Up _ _ = when (c == '\ESC') exitSuccess
onKeyMouse st key Down _ _ = do
    --putStrLn $ "key down: " ++ show key
    cst <- readIORef st
    let (d, phi, theta) = cameraSphericalPosition cst
        dps = dataPointScale cst

    let cst' = case key of
                    (Char 'z') -> cst { cameraDistance = min 3 (d * 1.1) } 
                    (Char 'a') -> cst { cameraDistance = max 0.1 (d * 0.9) } 
                    (SpecialKey KeyDown) -> cst { cameraTheta = theta - 0.1 } 
                    (SpecialKey KeyUp) -> cst { cameraTheta = theta + 0.1 } 
                    (SpecialKey KeyLeft) -> cst { cameraPhi = phi + 0.1 } 
                    (SpecialKey KeyRight) -> cst { cameraPhi = phi - 0.1 }
                    (Char '-') -> cst { dataPointScale = max 0.1 (dps * 0.9) }   
                    (Char '=') -> cst { dataPointScale = min 10 (dps * 1.1) }   
                    _ -> cst 

    --putStrLn $ "camera distance: " ++ show (cameraDistance cst) ++ " -> " ++ show (cameraDistance cst')
    writeIORef st cst'
    postRedisplay Nothing 
onKeyMouse _ _ _ _ _ = return ()

renderLoop :: Window -> IORef AppState -> [AppState -> IO ()] -> IO ()
renderLoop w st rs = do
    reshapeCallback $= Just reshape
    displayCallback $= render w st rs
    depthFunc $= Just Less
    clearColor $= Color4 0 0.1 0.2 1
    mainLoop
    postRedisplay $ Just w

reshape :: ReshapeCallback
reshape size = do 
  viewport $= (Position 0 0, size)

render :: Window -> IORef AppState -> [AppState -> IO ()] -> IO ()
render w st rs = do
    cst <- readIORef st

    --putStrLn $ "render, camera distance: " ++ show (cameraDistance cst)

    clear [ ColorBuffer, DepthBuffer ]
    matrixMode $= Modelview 0
    loadIdentity

    -- actual aspect ratio
    (Size sw sh) <- OpenGL.get windowSize
    let a = (realToFrac sw) / (realToFrac sh)

    -- vertical fov 
    let f = 2 * atanDeg(tanDeg(60 / 2) / a)

    perspective f a 0.001 10 

    let (x, y, z) = sphericalToCartesian (cameraDistance cst) (cameraPhi cst) (cameraTheta cst)

    cameraLookAt x y z 0 0 (0 :: GLfloat)

    sequence_ $ map ($ cst) rs

    reportErrors
    flush
    swapBuffers

