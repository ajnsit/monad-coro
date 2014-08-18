{-
    Copyright 2010-2012 Mario Blazevic

    This file is part of the Streaming Component Combinators (SCC) project.

    The SCC project is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
    License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
    version.

    SCC is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along with SCC.  If not, see
    <http://www.gnu.org/licenses/>.
-}

-- | A coroutine can choose to launch another coroutine. In this case, the nested coroutines always suspend to their
-- invoker. If a function from this module, such as 'pogoStickNested', is used to run a nested coroutine, the parent
-- coroutine can be automatically suspended as well. A single suspension can thus suspend an entire chain of nested
-- coroutines.
--
-- Nestable coroutines of this kind should group their suspension functors into an 'EitherFunctor'. A simple coroutine
-- suspension can be converted to a nested one using functions 'mapSuspension' and 'liftAncestor'. To run nested
-- coroutines, use 'pogoStickNested', or 'weave' with a 'NestWeaveStepper'.

{-# LANGUAGE ScopedTypeVariables, Rank2Types, MultiParamTypeClasses, TypeFamilies,
             FlexibleContexts, FlexibleInstances, OverlappingInstances, UndecidableInstances
 #-}

module Control.Monad.Coroutine.Nested
   (
      EitherFunctor(..), eitherFunctor, mapNestedSuspension,
      pogoStickNested,
      NestWeaveStepper,
      ChildFunctor(..), AncestorFunctor(..),
      liftParent, liftAncestor
   )
where

import Control.Monad (liftM)
import Control.Monad.Coroutine

-- | Combines two alternative functors into one, applying one or the other. Used for nested coroutines.
data EitherFunctor l r x = LeftF (l x) | RightF (r x)
instance (Functor l, Functor r) => Functor (EitherFunctor l r) where
   fmap f (LeftF l) = LeftF (fmap f l)
   fmap f (RightF r) = RightF (fmap f r)

-- | Like 'either' for the EitherFunctor data type.
eitherFunctor :: (l x -> y) -> (r x -> y) -> EitherFunctor l r x -> y
eitherFunctor left _ (LeftF f) = left f
eitherFunctor _ right (RightF f) = right f

-- | Change the suspension functor of a nested 'Coroutine'.
mapNestedSuspension :: (Functor s0, Functor s, Monad m) => (forall y. s y -> s' y) ->
                       Coroutine (EitherFunctor s0 s) m x -> Coroutine (EitherFunctor s0 s') m x
mapNestedSuspension f cort = Coroutine {resume= liftM map' (resume cort)}
   where map' (Right r) = Right r
         map' (Left (LeftF s)) = Left (LeftF $ fmap (mapNestedSuspension f) s)
         map' (Left (RightF s)) = Left (RightF (f $ fmap (mapNestedSuspension f) s))

-- | Run a nested 'Coroutine' that can suspend both itself and the current 'Coroutine'.
pogoStickNested :: forall s1 s2 m x. (Functor s1, Functor s2, Monad m) =>
                   (s2 (Coroutine (EitherFunctor s1 s2) m x) -> Coroutine (EitherFunctor s1 s2) m x)
                   -> Coroutine (EitherFunctor s1 s2) m x -> Coroutine s1 m x
pogoStickNested reveal t =
   Coroutine{resume= resume t
                      >>= \s-> case s
                               of Right result -> return (Right result)
                                  Left (LeftF s') -> return (Left (fmap (pogoStickNested reveal) s'))
                                  Left (RightF c) -> resume (pogoStickNested reveal (reveal c))}

-- | Type of functions capable of combining two child coroutines' 'CoroutineStepResult' values into a parent coroutine.
-- Use with the function 'weave'.
type NestWeaveStepper s0 s1 s2 m x y z = WeaveStepper (EitherFunctor s0 s1) (EitherFunctor s0 s2) s0 m x y z

-- | Class of functors that can contain another functor.
class Functor c => ChildFunctor c where
   type Parent c :: * -> *
   wrap :: Parent c x -> c x
instance (Functor p, Functor s) => ChildFunctor (EitherFunctor p s) where
   type Parent (EitherFunctor p s) = p
   wrap = LeftF

-- | Class of functors that can be lifted.
class (Functor a, Functor d) => AncestorFunctor a d where
   -- | Convert the ancestor functor into its descendant. The descendant functor typically contains the ancestor.
   liftFunctor :: a x -> d x

instance Functor a => AncestorFunctor a a where
   liftFunctor = id
instance (Functor a, ChildFunctor d, d' ~ Parent d, AncestorFunctor a d') => AncestorFunctor a d where
   liftFunctor = wrap . (liftFunctor :: a x -> d' x)

-- | Converts a coroutine into a child nested coroutine.
liftParent :: forall m p c x. (Monad m, Functor p, ChildFunctor c, p ~ Parent c) => Coroutine p m x -> Coroutine c m x
liftParent = mapSuspension wrap
{-# INLINE liftParent #-}

-- | Converts a coroutine into a descendant nested coroutine.
liftAncestor :: forall m a d x. (Monad m, Functor a, AncestorFunctor a d) => Coroutine a m x -> Coroutine d m x
liftAncestor = mapSuspension liftFunctor
{-# INLINE liftAncestor #-}

