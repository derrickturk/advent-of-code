{-# LANGUAGE FlexibleContexts #-}

import Control.Monad (forM_, replicateM, when)
import Control.Monad.ST
import Data.Array.ST
import Data.STRef

octopi :: String -> ST s (STUArray s (Int, Int) Int)
octopi input =
  let vals = fmap parseChar <$> lines input
      parseChar c = fromEnum c - fromEnum '0'
      cols = maximum $ length <$> vals
      rows = length vals
   in newListArray ((0, 0), (rows - 1, cols - 1)) $ concat vals

indices :: (MArray a e m) => a (Int, Int) e -> m [(Int, Int)]
indices arr = do
  ((i0, j0), (i1, j1)) <- getBounds arr
  pure [(i, j) | i <- [i0..i1]
               , j <- [j0..j1]
       ]

modifyInPlace :: (MArray a e m)
              => (e -> e)
              -> a (Int, Int) e
              -> m ()
modifyInPlace f arr = do
  ixs <- indices arr
  forM_ ixs $ \i -> do
    x <- readArray arr i
    writeArray arr i $ f x

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (i, j) = [(i + a, j + b) | a <- [(-1)..1]
                                   , b <- [(-1)..1]
                                   , not (a == 0 && b == 0)
                   ]

step :: STUArray s (Int, Int) Int -> ST s Int
step grid = do
  modifyInPlace (+ 1) grid
  flashed <- mapArray (> 9) grid
  whichFlashed <- fmap fst . filter snd <$> getAssocs flashed
  forM_ whichFlashed $ \i -> do
    let ns = neighbors i
    forM_ ns $ doFlash grid flashed
    pure ()
  whichFlashed' <- fmap fst . filter snd <$> getAssocs flashed
  forM_ whichFlashed' $ \i -> do
    writeArray grid i 0
  -- getAssocs grid >>= traceM . show
  pure $ length whichFlashed'
  where
    doFlash g f'ed i = do
      bounds <- getBounds g
      when (inRange bounds i) $ do
        v <- readArray g i
        let v' = v + 1
        writeArray g i v'
        alreadyFd <- readArray f'ed i
        when (v' > 9 && not alreadyFd) $ do
          writeArray f'ed i True
          forM_ (neighbors i) $ doFlash g f'ed

countFlashes :: String -> Int -> ST s Int
countFlashes input steps = do
  grid <- octopi input
  n <- newSTRef 0
  _ <- replicateM steps $ do
    n' <- step grid
    modifySTRef n (+ n')
  readSTRef n

stepsToSync :: String -> ST s Int
stepsToSync input = do
  grid <- octopi input
  target <- length <$> indices grid
  go 0 grid target
  where
    go n g t = do
      flashed <- step g
      if flashed == t
        then pure (n + 1)
        else go (n + 1) g t

main :: IO ()
main = do
  input <- getContents
  print $ runST $ countFlashes input 100
  print $ runST $ stepsToSync input
