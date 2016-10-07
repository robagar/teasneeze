module State where

import Spherical

data AppState = AppState {
        cameraDistance :: Float,
        cameraPhi :: Float,
        cameraTheta :: Float
    }

cameraSphericalPosition :: AppState -> SphericalCoords
cameraSphericalPosition s = (cameraDistance s, cameraPhi s, cameraTheta s)