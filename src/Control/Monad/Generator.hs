{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Control.Monad.Generator (
  Gen, gen,
  genCoro, genX,
  yield, fmapM, fmapV,
  listToGen, genToList
) where

import Control.Monad (liftM)
import Control.Monad.Free

------------------------------
-- MONADIC VALUE GENERATORS --
------------------------------

-- Generator suspension functor
data GenS m x c =
  GenS {
    -- | The monadic value generated
    genX :: m x,
    -- | The continuation
    genC :: c
  }

instance Functor (GenS m x) where
  fmap f gs = GenS (genX gs) (f $ genC gs)

-- A Generator which generates monadic values
type Gen m x = Free (GenS m x) ()


-----------------------
-- BASIC COMBINATORS --
-----------------------

-- Yield a monadic value
yield :: Monad m => m x -> Gen m x
yield mx = Free $ GenS mx (return ())

-- Alias to hide actual constructor
gen :: m x -> c -> GenS m x c
gen = GenS

-- An fmap that transforms monadic values
fmapM :: Monad m => (m x -> m x1) -> Gen m x -> Gen m x1
fmapM _ (Pure fa) = Pure fa
fmapM f (Free (GenS gx gc)) = Free $ GenS (f gx) (fmapM f gc)

-- An fmap that transforms output of monadic values
fmapV :: Monad m => (x -> x1) -> Gen m x -> Gen m x1
fmapV f = fmapM (liftM f)

-- A Monadic List to Generator conversion
-- Sequentially generates all monadic values in the list
listToGen :: Monad m => [m x] -> Gen m x
listToGen = foldr ((>>) . yield) (return ())

-- A Generator to Monadic List conversion
genToList :: Monad m => Gen m x -> m [m x]
genToList (Pure _) = return []
genToList (Free (GenS gx gc)) = liftM (gx:) $ genToList gc


