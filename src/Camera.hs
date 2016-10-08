module Camera where

import Graphics.Rendering.OpenGL

cameraLookAt :: Real a => a -> a -> a -> a -> a -> a -> IO ()
cameraLookAt ex ey ez ax ay az = lookAt
    (Vertex3 (f ex) (f ey) (f ez))
    (Vertex3 (f ax) (f ay) (f az)) 
    (Vector3 0 1 0)
    where f = realToFrac

    