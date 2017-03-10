module State where

import Spherical
import Util

data AppState = AppState {
        initializingTSNE :: Bool,
        runningTSNE :: Bool,
        cameraDistance :: Float,
        cameraPhi :: Float,
        cameraTheta :: Float,
        dataPointScale :: Float,
        dataPointPositions :: [Vec3]
    }

cameraSphericalPosition :: AppState -> SphericalCoords
cameraSphericalPosition s = (cameraDistance s, cameraPhi s, cameraTheta s)