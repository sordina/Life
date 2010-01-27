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
import Random
import Control.Monad

-- Internal imports

import LifeBool
import LifeRendering
import LifeStructures

main :: IO ()
main = do
  -- _ <- getArgsAndInitialize
  lifeList <- liftM (iterate nextSnapshot) createSnapshot
  lifeListIO <- newIORef lifeList
  showLifeList lifeList
  -- window "LIFE" 100 100 (display lifeList)
  -- mainLoop

showLifeList list = mapM_ putStrLn lines
  where
    lines = take 20 flatSnaps
    flatSnaps = map ((show . take 10) . toFlatRaw) list

createSnapshot :: IO HealthSnapshot
createSnapshot = liftM mkSnapshot $ mkNestedBools 100
  where
    mkBools n = sequence $ replicateM n getStdRandom (randomR (False,True))
    mkNestedBools n = replicateM n (mkBools n)

window title width height display = do
  createWindow title
  windowSize $= Size width height
  displayCallback $= display

-- Pops off the head of the life-list and renders it.
display :: IORef [HealthSnapshot] -> IO ()
display lifeList = do
  clear [ColorBuffer]
  snap:snaps <- readIORef lifeList
  writeIORef lifeList snaps
  renderSnapshot snap
  flush
