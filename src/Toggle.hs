module Toggle (toggle, toggleN, toggleBool) where

import Data.IORef
import Data.StateVar (get,($=))

-- Returns a function that cycles two IO functions for each call.
toggle :: IO () -> IO () -> IO (IO ())
toggle fa fb = toggleN [fa, fb]

-- Returns a function that cycles N IO functions for each call.
toggleN :: [IO ()] -> IO (IO ())
toggleN fl = do
  list <- newIORef $ cycle fl
  return $ f list
  where
    f listIO = do
      (h:t) <- get listIO
      listIO $= t
      h

-- Returns a function that chooses between two IO functions depending on the result of a third.
-- TODO: Decide exactly how this is meant to operate.
toggleBool :: IO Bool -> IO () -> IO () -> IO (IO ())
toggleBool switch fa fb = do
  list <- newIORef $ cycle [fa, fb]
  return $ f list
  where
    f listIO = do
      perform <- switch
      (h:t) <- get listIO

      if perform
         then listIO $= t >> h
         else h
