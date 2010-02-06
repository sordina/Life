-- External imports
import Graphics.UI.GLUT
import Data.IORef
import Data.Time.Clock.POSIX
import Data.Maybe (fromMaybe)
import Safe (readDef)
import System.Exit
--import Control.Monad (when)

-- Internal imports
import LifeMatrix
import LifeRendering
import Toggle

main :: IO ()
main = do
  gameSize <- (return . getSize . snd) =<< getArgsAndInitialize

  initialDisplayMode $= [DoubleBuffered]
  lifeList <- randomGame gameSize
  lifeListIO <- newIORef lifeList
  timeIO <- newIORef (0::POSIXTime) -- Epoch

  window "LIFE" gameSize (display lifeListIO timeIO)

  mainLoop

getSize :: [String] -> Integer
getSize [size] = readDef 50 size
getSize _ = 50

window :: String -> Integer -> IO () -> IO ()
window title gameSize displayCB = do
  createWindow title
  smallSize <- get windowSize

  -- Toggles betweenrequiring keyboard input and automatically running.
  auto <- toggle (idleCallback $= Just displayCB) (idleCallback $= Nothing)

  displayCallback $= displayCB
  keyboardMouseCallback $= Just (mkKM smallSize displayCB auto)

  translate (Vector3 (negate 1) (negate 1) (0::GLfloat))
  scale s s (0::GLfloat) -- Should tie this to the board size
  where s = 2 / fromIntegral gameSize

-- Pops off the head of the life-list and renders it at second intervals.
display :: IORef [LifeSnapshot] -> IORef POSIXTime -> IO ()
display lifeList timeIO = do
  previousTime <- get timeIO
  --currentTime <- getPOSIXTime

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

mkKM smallSize displayCB auto = km
  where
    km :: Key -> KeyState -> c -> d -> IO ()
    km (Char 'n') Down _ _ = displayCB
    km (Char 'j') Down _ _ = displayCB
    km (Char 'f') Down _ _ = fullscreen smallSize
    km (Char 'q') Down _ _ = exitWith ExitSuccess
    km (Char 'g') Down _ _ = auto
    km _ _ _ _ = return ()

fullscreen smallSize = do
  currentSize <- get windowSize
  screenSize <- get screenSize
  if currentSize == screenSize
    then cursor $= LeftArrow >> windowSize $= smallSize
    else cursor $= None      >> fullScreen
