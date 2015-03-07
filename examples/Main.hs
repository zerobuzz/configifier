{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE OverlappingInstances                     #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE PolyKinds                                #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE StandaloneDeriving                       #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE UndecidableInstances                     #-}

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
  :| "blu" :> SubCfg :>: "description of blu"
  :| "uGH" :> [Cfg'] :>: "...  and something about uGH"

type Cfg' =
     "go"  :> ST
  :| "blu" :> SubCfg

type SubCfg =
     "lii" :> Bool :>: "lii-desc"


-- | you can write sample configs in haskell syntax.  (it's not very
-- convenient, so luckily it's not usually required either.  see test
-- suite on how to access fields in cfg from here on.)
defaultCfg :: Cfg
defaultCfg =
     entry 3
  :| entry False
  :| entry [ entry "drei" :| entry True
           , entry "vier" :| entry False
           ]


main :: IO ()
main = do
    sources <- sequence
        [ ConfigFileYaml <$> SBS.readFile "examples/config.yaml"
        , ShellEnv       <$> getEnvironment
        , CommandLine    <$> getArgs
        ]

    -- putStrLn $ ppShow sources

    ST.putStrLn $ docs (Proxy :: Proxy Cfg)

    case configify sources of
        Left e -> print e
        Right (cfg :: Cfg) -> do
            putStrLn $ ppShow cfg
            putStrLn . cs . Yaml.encode $ cfg

            putStrLn "accessing config values:"
            print $ (cfg >. (Proxy :: Proxy '["bla"]))
            print $ (cfg >. (Proxy :: Proxy '["blu", "lii"]))
            print $ (>. (Proxy :: Proxy '["go"])) <$> (cfg >. (Proxy :: Proxy '["uGH"]))
