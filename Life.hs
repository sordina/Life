-- External imports
import Graphics.UI.GLUT
import Data.IORef
import Data.Time.Clock.POSIX
import System.Exit
--import Control.Monad (when)

-- Internal imports
import LifeMatrix
import LifeRendering

main :: IO ()
main = do
  (program, args) <- getArgsAndInitialize

  gameSize <- return $ if length args == 1
                        then (read :: String -> Integer) $ head args
                        else 50

  initialDisplayMode $= [DoubleBuffered]
  lifeList <- randomGame gameSize
  lifeListIO <- newIORef lifeList
  timeIO <- newIORef (0::POSIXTime) -- Epoch

  window "LIFE" gameSize (display lifeListIO timeIO)

  mainLoop

window :: String -> Integer -> IO () -> IO ()
window title gameSize displayCB = do
  createWindow title
  smallSize <- get windowSize

  displayCallback $= displayCB
  keyboardMouseCallback $= Just (mkKM smallSize displayCB)

  translate (Vector3 (negate 1) (negate 1) (0::GLfloat))
  scale s s (0::GLfloat) -- Should tie this to the board size
  where s = 2 / fromIntegral gameSize

-- Pops off the head of the life-list and renders it at second intervals.
display :: IORef [LifeSnapshot] -> IORef POSIXTime -> IO ()
display lifeList timeIO = do
  previousTime <- get timeIO
  currentTime <- getPOSIXTime

  -- leave this until we get propper rendering going
  -- when (currentTime > (previousTime + 1)) $ do
  do
    timeIO $= (previousTime + 1)

    clear [ColorBuffer]
    snap:snaps <- get lifeList
    --putStrLn $ show snap
    lifeList $= snaps
    renderSnapshot snap
    --renderBounds
    flush
    swapBuffers

mkKM smallSize displayCB = km
  where
    km :: Key -> KeyState -> c -> d -> IO ()
    km (Char 'n') Down _ _ = displayCB
    km (Char 'j') Down _ _ = displayCB
    km (Char 'f') Down _ _ = fullscreen smallSize
    km (Char 'q') Down _ _ = exitWith ExitSuccess
    km _ _ _ _ = return ()

fullscreen smallSize = do
  currentSize <- get windowSize
  screenSize <- get screenSize
  if currentSize == screenSize
    then cursor $= LeftArrow >> windowSize $= smallSize
    else cursor $= None      >> fullScreen
