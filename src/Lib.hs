module Lib
  ( totalize
  ) where

import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

{- $setup
>>> import Data.List.NonEmpty (NonEmpty(..))
>>> import qualified Data.Set as Set
>>> import Test.QuickCheck
>>> import Test.QuickCheck.Function
>>> :{
instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary
:}

>>> :{
instance CoArbitrary a => CoArbitrary (NonEmpty a) where
  coarbitrary = coarbitrary . toList
:}

>>> :{
instance Function a => Function (NonEmpty a) where
  function = functionMap (\(x :| xs) -> (x, xs)) (uncurry (:|))
:}
-}

{-| FIXME: Find a better name: `onNonEmpty`? Maybe rather `onFoldable`?

==== __Examples:__
>>> totalize maximum []
Nothing
>>> totalize maximum (Set.singleton 3)
Just 3

==== __Properties:__
prop> \(Fun _ f) -> totalize f [] == Nothing

prop> \(Fun _ f) xs -> totalize f xs == Just (f xs)
-}
totalize :: Foldable f => (NonEmpty a -> b) -> f a -> Maybe b
totalize f = fmap f . NonEmpty.nonEmpty . toList
