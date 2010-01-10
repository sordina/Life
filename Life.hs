{--
    http://en.wikipedia.org/wiki/Conway's_Game_of_Life

    Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
    Any live cell with more than three live neighbours dies, as if by overcrowding.
    Any live cell with two or three live neighbours lives on to the next generation.
    Any dead cell with exactly three live neighbours becomes a live cell.
--}

import Graphics.UI.GLUT
import Data.IORef
import Random
import Control.Monad

-- import Matrix

type Health = Bool

nextState :: Health -> Integer -> Health

nextState True neighbours
  | neighbours < 2 = False
  | neighbours > 3 = False
  | otherwise      = True

nextState False neighbours
  | neighbours == 3 = True
  | otherwise       = False

main = do
  (progname, _) <- getArgsAndInitialize
  initialSnapshopt <- createSnapshot
  lifeList <- newIORef $ iterate lifeTransformer initialSnapshopt
  window "LIFE" 100 100 (display lifeList)
  mainLoop

createSnapshot :: IO LifeSnapshot
createSnapshot = liftM mkLifeSnapshot $ mkNestedBools 100
  where
    mkBools n = sequence $ replicateM n getStdRandom (randomR (False,True))
    mkNestedBools n = sequence $ replicate n (mkBools n)

lifeTransformer :: LifeSnapshot -> LifeSnapshot
lifeTransformer = fmap succeed 

data LifeSnapshot = LifeSnapshot {
    cells::[[Bool]],
    width::Integer,
    height::Integer
  }

mkLifeSnapshot cells = LifeSnapshot {
    cells = cells,
    width = len (head cells),
    height = len cells
  }

succeed :: LifeCell -> LifeCell
succeed = undefined

parity :: LifeSnapshot -> Integer -> Integer -> Bool
parity life x y = deduce $ neighbours life x y
  where
    deduce = even . length . (filter id)

neighbours :: LifeSnapshot -> Integer -> Integer -> [Bool]
neighbours life x y = undefined

{- Utility functions -}

len = fromIntegral . length

window title width height display = do
  createWindow title
  windowSize $= Size width height
  displayCallback $= display

-- Pops off the head of the life-list and renders it.
display :: IORef [LifeSnapshot] -> IO ()
display lifeList = do
  clear [ColorBuffer]
  snap:snaps <- readIORef lifeList
  writeIORef lifeList snaps
  renderSnapshot snap
  flush

renderSnapshot :: LifeSnapshot -> IO ()
renderSnapshot = undefined
