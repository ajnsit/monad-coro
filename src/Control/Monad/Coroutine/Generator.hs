{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Control.Monad.Coroutine.Generator (
  Gen, gen,
  genCoro, genX,
  yield, fmapM, fmapV,
  listToGen, genToList
) where

import Control.Monad (liftM)
import Control.Monad.Coroutine ( Coroutine(..), suspend, resume )

------------------------------
-- MONADIC VALUE GENERATORS --
------------------------------

-- Suspension functor for a generator
data GenS m x coro =
  GenS {
    genCoro :: coro,
    genX    :: m x
  }

instance Functor (GenS m x) where
  fmap f gs = GenS (f $ genCoro gs) (genX gs)

-- A Generator which generates monadic values
type Gen m x a = Coroutine (GenS m x) m a

-- Yield a monadic value
yield :: Monad m => m x -> Gen m x ()
yield mx = suspend $ GenS (return ()) mx

-- Alias to hide actual constructor
gen :: coro -> m x -> GenS m x coro
gen = GenS

-- An fmap that transforms monadic values
fmapM :: Monad m => (m x -> m x1) -> Gen m x o -> Gen m x1 o
fmapM f g = Coroutine $ do
  gs <- resume g
  case gs of
    Right a -> return (Right a)
    Left gs' -> resume $ suspend $ GenS (fmapM f $ genCoro gs') (f $ genX gs')

-- An fmap that transforms output of monadic values
fmapV :: Monad m => (x -> x1) -> Gen m x o -> Gen m x1 o
fmapV f = fmapM (liftM f)

-- A Monadic List to Generator conversion
-- Sequentially generates all monadic values in the list
listToGen :: Monad m => [m x] -> Gen m x ()
listToGen = foldr ((>>) . yield) (return ())

-- A Generator to Monadic List conversion
-- Discards the generator return value
genToList :: Monad m => Gen m x o -> m [m x]
genToList g = do
    gs <- resume g
    case gs of
      Right _ -> return []
      Left gs' -> do
        xs <- genToList $ genCoro gs'
        return (genX gs':xs)


