{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TypeOperators                            #-}

{-# OPTIONS  #-}

module Main
where

import Control.Applicative
import Data.String.Conversions
import Data.Typeable
import Prelude
import Text.Show.Pretty (ppShow)
import System.Environment

import qualified Data.ByteString as SBS
import qualified Data.Text.IO as ST
import qualified Data.Yaml as Yaml

import Data.Configifier


-- * the config types

type Cfg =
     "bla" :> Int
  :| "blu" :>: "description of blu" :> SubCfg
  :| "uGH" :>: "...  and something about uGH" :> [Cfg']

type Cfg' =
     "go"  :> ST
  :| "blu" :> SubCfg

type SubCfg =
     "lii" :>: "lii-desc" :> Bool


-- | you can write sample configs in haskell syntax.  (it's not very
-- convenient, so luckily it's not usually required either.  see test
-- suite on how to access fields in cfg from here on.)
defaultCfg :: Cfg
defaultCfg =
     Proxy :> 3
  :| Proxy :>: Proxy :> (Proxy :>: Proxy :> False)
  :| Proxy :>: Proxy :> [Proxy :> "drei" :| Proxy :> Proxy :>: Proxy :> True, Proxy :> "vier" :| Proxy :> Proxy :>: Proxy :> False]


main :: IO ()
main = do
    sources <- sequence
        [ ConfigFile  <$> SBS.readFile "examples/config.yaml"
        , ShellEnv    <$> getEnvironment
        , CommandLine <$> getArgs
        ]

    -- putStrLn $ ppShow sources

    ST.putStrLn $ docs (Proxy :: Proxy Cfg)

    case configify sources of
        Left e -> print e
        Right (cfg :: Cfg) -> do
            putStrLn $ ppShow cfg
            putStrLn . cs . Yaml.encode $ cfg
