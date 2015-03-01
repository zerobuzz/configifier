{-# LANGUAGE BangPatterns                             #-}
{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE DeriveFunctor                            #-}
{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE ExistentialQuantification                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving               #-}
{-# LANGUAGE InstanceSigs                             #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE NoImplicitPrelude                        #-}
{-# LANGUAGE OverlappingInstances                     #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE PackageImports                           #-}
{-# LANGUAGE PatternGuards                            #-}
{-# LANGUAGE PolyKinds                                #-}
{-# LANGUAGE QuasiQuotes                              #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE StandaloneDeriving                       #-}
{-# LANGUAGE TemplateHaskell                          #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE TypeSynonymInstances                     #-}
{-# LANGUAGE ViewPatterns                             #-}

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
import qualified Data.Yaml as Yaml

import Data.Configifier


-- * the config types

type Cfg =
     "bla" :>: "description of bla" :> Int
  :| "blu" :> SubCfg
  :| "uGH" :>: "...  and something about uGH" :> [Cfg']

type Cfg' =
     "go"  :> ST
  :| "blu" :> SubCfg

type SubCfg =
     "lii" :> Bool


-- | you can write sample configs in haskell syntax.  (it's not very
-- convenient, so luckily it's not usually required either.  see test
-- suite on how to access fields in cfg from here on.)
defaultCfg :: Cfg
defaultCfg =
     Proxy :>: Proxy :> 3
  :| Proxy :> (Proxy :> False)
  :| Proxy :>: Proxy :> [Proxy :> "drei" :| Proxy :> Proxy :> True, Proxy :> "vier" :| Proxy :> Proxy :> False]


main :: IO ()
main = do
    sources <- sequence
        [ ConfigFile  <$> SBS.readFile "examples/config.yaml"
        , ShellEnv    <$> getEnvironment
        , CommandLine <$> getArgs
        ]

    let cfg :: Either Error Cfg = configify sources

    putStrLn $ ppShow (sources, cfg)
    putStrLn . either show (cs . Yaml.encode) $ cfg
