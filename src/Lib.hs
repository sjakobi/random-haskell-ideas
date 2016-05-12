module Lib where

import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

{- $setup
>>> import Test.QuickCheck
-}

{-|
>>> totalize maximum []
Nothing

prop> \(NonEmpty xs) -> totalize maximum xs == Just (maximum xs :: Int)

-}
totalize :: Foldable f => (NonEmpty a -> b) -> f a -> Maybe b
totalize f = fmap f . NonEmpty.nonEmpty . toList
