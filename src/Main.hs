module Main where

import System.Exit (exitSuccess)
import Control.Monad (forever, when)
import Graphics.UI.GLUT
import qualified Graphics.Rendering.OpenGL as OpenGL
import Graphics.Rendering.OpenGL.Util
import Data.IORef

import Spherical
import State
import DataPoints
import LoadData
import Util

main :: IO ()
main = do
    w <- initRenderWindow "Tea Sneeze"
    b <- prepareRenderOutlineBox
    dps <- require <$> loadData "test/digits/digits_tsne.json"
    dprs <- prepareRenderDataPoints dps
    st <- newIORef $ AppState 2 0 0

    keyboardMouseCallback $= Just (onKeyMouse st)

    renderLoop w st ([b] ++ dprs) 

prepareRenderOutlineBox :: IO (IO ())
prepareRenderOutlineBox = do
    p <- require <$> loadProgram "shaders/basic_model_view_projection.vs" "shaders/flat_green.fs"
    return $ render p
    where
    render p = preservingMatrix $ do
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

    initialWindowSize      $= Size 640 480

    w <- createWindow title

    --keyboardUpCallback $= Just quitOnEsc

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

    let cst' = case key of
                    (Char 'z') -> cst { cameraDistance = d * 1.1 } 
                    (Char 'a') -> cst { cameraDistance = d * 0.9 } 
                    (SpecialKey KeyDown) -> cst { cameraTheta = theta + 0.1 } 
                    (SpecialKey KeyUp) -> cst { cameraTheta = theta - 0.1 } 
                    (SpecialKey KeyLeft) -> cst { cameraPhi = phi + 0.1 } 
                    (SpecialKey KeyRight) -> cst { cameraPhi = phi - 0.1 } 
                    _ -> cst 

    --putStrLn $ "camera distance: " ++ show (cameraDistance cst) ++ " -> " ++ show (cameraDistance cst')
    writeIORef st cst'
    postRedisplay Nothing 
onKeyMouse _ _ _ _ _ = return ()

renderLoop :: Window -> IORef AppState -> [IO ()] -> IO ()
renderLoop w st rs = do
    reshapeCallback $= Just reshape
    displayCallback $= render w st rs
    depthFunc $= Just Less
    mainLoop
    postRedisplay $ Just w

reshape :: ReshapeCallback
reshape size = do 
  viewport $= (Position 0 0, size)

render :: Window -> IORef AppState -> [IO ()] -> IO ()
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

    lookEyeAt x y z 0 0 (0 :: GLfloat)

    sequence_ rs

    reportErrors
    flush
    swapBuffers

    -- postRedisplay $ Just w


lookEyeAt :: Real a => a -> a -> a -> a -> a -> a -> IO ()
lookEyeAt ex ey ez ax ay az = OpenGL.lookAt
    (Vertex3 (f ex) (f ey) (f ez))
    (Vertex3 (f ax) (f ay) (f az)) 
    (Vector3 0 1 0)
    where f = realToFrac

tanDeg :: Floating a => a -> a
tanDeg = tan.deg2rad

atanDeg :: Floating a => a -> a
atanDeg = rad2deg.atan

rad2deg :: Floating a => a -> a 
rad2deg = (180*).(/pi)

deg2rad :: Floating a => a -> a 
deg2rad =  (pi*).(/180)
