{--
    http://en.wikipedia.org/wiki/Conway's_Game_of_Life

    Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
    Any live cell with more than three live neighbours dies, as if by overcrowding.
    Any live cell with two or three live neighbours lives on to the next generation.
    Any dead cell with exactly three live neighbours becomes a live cell.
--}

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

window title width height display = do
  createWindow title
  windowSize $= Size width height
  displayCallback $= display
  keyboardMouseCallback $= Just km

  translate (Vector3 (0-1) (0-1) (0::GLfloat))
  scale 0.04 0.04 (0::GLfloat)

-- Pops off the head of the life-list and renders it at second intervals.
display :: IORef [LifeSnapshot] -> IORef POSIXTime -> IO ()
display lifeList timeIO = do
  previousTime <- get timeIO
  currentTime <- getPOSIXTime

  when (currentTime > (previousTime + 1)) $ do
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

km :: Key -> KeyState -> c -> d -> IO ()
km (Char 'f') Down _ _ = fullscreen
km _ _ _ _ = return ()

fullscreen :: IO ()
fullscreen = do
  print "Entering fullscreen mode."
  fullScreenToggle
