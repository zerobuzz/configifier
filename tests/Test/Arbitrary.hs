{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Arbitrary () where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson
import Data.CaseInsensitive
import Data.Scientific
import Data.String.Conversions
import Test.QuickCheck

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector


-- | 25 most common adjectives according to the Oxford English
-- Dictionary.
readableStrings :: [String]
readableStrings =
    "good" : "new" : "first" : "last" : "long" : "great" : "little" :
    "own" : "other" : "old" : "right" : "big" : "high" : "different" :
    "small" : "large" : "next" : "early" : "young" : "important" :
    "few" : "public" : "bad" : "same" : "able" :
    []

instance Arbitrary ST where
    arbitrary = cs <$> elements readableStrings

instance Arbitrary (CI ST) where
    arbitrary = mk <$> arbitrary

instance Arbitrary Value where
    arbitrary = sized $ \ size -> let size' = size `div` 2 in oneof
        [ return Null
        , String . cs <$> elements readableStrings
        , Number <$> (scientific <$> arbitrary <*> oneof [return 0, (`mod` 12) <$> arbitrary])
        , Bool <$> arbitrary
        , Array . Vector.fromList <$> resize size' arbitrary
        , object <$> resize size' arbitrary
        ]

    shrink (Array v) = Array . Vector.fromList <$> shrink (Vector.toList v)
    shrink (Object m) = Object . HashMap.fromList <$> shrink (HashMap.toList m)
    shrink _ = []
