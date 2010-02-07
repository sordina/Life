import Matrix
import qualified ArrayMatrix  as A1
import qualified ArrayMatrix2 as A2

main = do
  profile A1.fromRows
  profile A2.fromRows

profile :: (Show a, Matrix m a) => ([[a]] -> m a) -> IO ()
profile = undefined
