-- External imports
import Graphics.UI.GLUT
import Data.IORef
import Data.Time.Clock.POSIX
import Control.Monad (when)

-- Internal imports
import LifeMatrix
import LifeRendering

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]

  lifeList <- randomGame 50
  lifeListIO <- newIORef lifeList
  timeIO <- newIORef =<< getPOSIXTime

  window "LIFE" 200 200 (display lifeListIO timeIO)

  mainLoop

window title width height displayCB = do
  createWindow title
  windowSize $= Size width height
  displayCallback $= displayCB
  keyboardMouseCallback $= Just (mkKM displayCB)

  translate (Vector3 (0-1) (0-1) (0::GLfloat))
  scale 0.04 0.04 (0::GLfloat) -- Should tie this to the board size

-- Pops off the head of the life-list and renders it at second intervals.
display :: IORef [LifeSnapshot] -> IORef POSIXTime -> IO ()
display lifeList timeIO = do
  previousTime <- get timeIO
  currentTime <- getPOSIXTime

  -- leave this until we get propper rendering going
  -- when (currentTime > (previousTime + 1)) $ do
  do
    print currentTime
    timeIO $= (previousTime + 1)

    clear [ColorBuffer]
    snap:snaps <- get lifeList
    --putStrLn $ show snap
    lifeList $= snaps
    renderSnapshot snap
    --renderBounds
    flush
    swapBuffers

mkKM displayCB = km
  where
    km :: Key -> KeyState -> c -> d -> IO ()
    km (Char 'n') Down _ _ = displayCB
    km (Char 'q') Down _ _ = exit
    km _ _ _ _ = return ()
