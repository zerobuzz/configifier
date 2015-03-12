{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE IncoherentInstances                      #-}
{-# LANGUAGE OverlappingInstances                     #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE Rank2Types                               #-}
{-# LANGUAGE GADTs                                    #-}

{-# OPTIONS  #-}

module Data.ConfigifierSpec
where

import Control.Applicative
import Control.Monad.Identity
import Data.Proxy
import Data.String.Conversions
import Debug.Trace
import Prelude
import Test.Hspec
import Test.QuickCheck
import Text.Show.Pretty

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson

import Data.Configifier

import Test.Arbitrary ()


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = describe "Configifier" $ do
    misc

misc :: Spec
misc = describe "misc" $ do
    undefined



{-


(use type classes and type families to make the following tests more
generic.)


-- test 1

type X1 = ToConfigCode (Maybe ("a" :> Int))
type X2 = Option (Label "a" (Type Int))

x1 :: (X1 ~ X2) => ToConfig X2 Maybe
x1 = Just (Just 3)

x2 = freeze (Tagged x1 :: Tagged X1 (ToConfig X1 Maybe))

x3 :: (X1 ~ X2) => ToConfig X2 Maybe
x3 = Nothing

x4 = freeze (Tagged x3 :: Tagged X1 (ToConfig X1 Maybe))


-- test 2

type X1 = ToConfigCode (Maybe ("a" :> Int))
type X2 = Option (Label "a" (Type Int))

x1 :: (X1 ~ X2) => ToConfig X2 Maybe
x1 = Just (Just 3)

x2 = freeze (Tagged x1 :: Tagged X1 (ToConfig X1 Maybe))

x3 :: (X1 ~ X2) => ToConfig X2 Maybe
x3 = Nothing

x4 = freeze (Tagged x3 :: Tagged X1 (ToConfig X1 Maybe))


-- test 3

type Cfg = ToConfigCode Cfg'

type Cfg' =
            "frontend"      :> ServerCfg
  :| Maybe ("backend"       :> ServerCfg)

type ServerCfg =
            "bind_port"   :> Int
  :| Maybe ("expose_host" :> ST)

x1 :: ToConfig Cfg Maybe
x1 = Just (Just 3 :| Just (Just "host")) :| Just (Just (Just 4 :| Just (Just "hist")))

x2 = freeze (Tagged x1 :: Tagged Cfg (ToConfig Cfg Maybe))

x3 :: ToConfig Cfg Maybe
x3 = Just (Just 3 :| Just (Just "host")) :| Nothing

x4 :: Tagged Cfg (ToConfig Cfg Identity)
x4 = case freeze (Tagged x3 :: Tagged Cfg (ToConfig Cfg Maybe)) of
    Right x -> x

x5 = SBS.putStr . (<> "\n") $ renderConfigFile x4

x6 = (parseConfigFile :: SBS -> Either Error (Tagged Cfg (ToConfig Cfg Maybe))) $ renderConfigFile x4

-}



{-
    describe "(>.)" $ do
        it "works (unit)" $ do
            (cfg >. (Proxy :: Proxy '["bla"])) `shouldBe` 3
            (cfg >. (Proxy :: Proxy '["blu", "lii"])) `shouldBe` False
            (>. (Proxy :: Proxy '["go"])) <$> (cfg >. (Proxy :: Proxy '["uGH"])) `shouldBe` ["drei","vier"]

    describe "FromJSON, ToJSON" $ do
        it "is mutually inverse" $ do
            simpleJSONTest False cfg
            simpleJSONTest False subCfg
            sequence_ (simpleJSONTest False <$> cfg's)

    describe "parseShellEnv" $ do
        it "works" $ do
            parseShellEnv (Proxy :: Proxy ("bla" :> Int)) [("BLA", "3")] `shouldBe`
                (Right $ Aeson.object ["bla" Aeson..= Aeson.Number 3.0])

simpleJSONTest :: (Eq a, Aeson.FromJSON a, Aeson.ToJSON a, Show a) => Bool -> a -> IO ()
simpleJSONTest noisy x = _trace $ x' `shouldBe` Aeson.Success x
  where
    x' = Aeson.fromJSON $ Aeson.toJSON x
    _trace = if noisy
        then trace $ cs (Aeson.encodePretty x) ++ "\n" ++ ppShow (x, x')
        else id

type Cfg =
     "bla" :> Int :>: "description of bla"
  :| "blu" :> SubCfg
  :| "uGH" :> [Cfg'] :>: "...  and something about uGH"

type Cfg' =
     "go"  :> ST
  :| "blu" :> SubCfg

type SubCfg =
     "lii" :> Bool

cfg :: Cfg
cfg =
     entry 3
  :| entry False  -- Curiously, it's not ok to put 'subCfg' here.
  :| entry cfg's

cfg's :: [Cfg']
cfg's =
    [ entry "drei" :| entry True
    , entry "vier" :| entry False
    ]

subCfg :: SubCfg
subCfg = entry False

-}
