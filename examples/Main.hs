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
import Control.Monad.Identity
import Data.Aeson (toJSON)
import Data.String.Conversions
import Data.Typeable
import Prelude
import System.Environment
import Text.Show.Pretty (ppShow)

import qualified Data.ByteString as SBS
import qualified Data.Text.IO as ST
import qualified Data.Yaml as Yaml

import Data.Configifier


-- * simple examples


ex0 :: (cfg ~ ToConfigCode ("bla" :> Int), t ~ ToConfig cfg Identity) => Tagged cfg t
ex0 = Tagged $ Identity 3

ex1 :: (cfg ~ ToConfigCode ("bla" :> Int), t ~ ToConfig cfg Maybe) => Tagged cfg t
ex1 = Tagged (Just 3)

ex2 :: (cfg ~ ToConfigCode ("lub" :> Bool :| "bla" :> Int), t ~ ToConfig cfg Maybe) => Tagged cfg t
ex2 = (Tagged $ Just False :| Just 3)

ex3 :: (cfg ~ ToConfigCode ("bla" :> Int :| "lub" :> [Bool]), t ~ ToConfig cfg Maybe) => Tagged cfg t
ex3 = (Tagged $ Just 3 :| Just [False, True])

ex4 :: ( ToConfig (NoDesc (ToConfigCode ("bla" :> Int :>: "dwfw"))) Maybe ~
         ToConfig (NoDesc (ToConfigCode ("bla" :> Int)))            Maybe
       , cfg ~ NoDesc (ToConfigCode ("bla" :> Int :>: "dwfw"))
       , t ~ ToConfig cfg Maybe
       ) => Tagged cfg t
ex4 = Tagged $ Just 3

test = putStrLn $ ppShow (renderConfigFile ex0, toJSON ex1, toJSON ex2, toJSON ex3, toJSON ex4)


-- * an interesting example

type Cfg = ToConfigCode Cfg'

type Cfg' =
     "frontend" :> ServerCfg :>: "descr"
  :| "backend" :> ServerCfg
  :| "default_users" :> [UserCfg] :>: "list of users that are created on start if database is empty"

type ServerCfg =
     "bind_port"   :> Int
  :| "bind_host"   :> ST
  :| "expose_host" :> Maybe ST

type UserCfg =
     "name"     :> ST :>: "user name (must be unique)"
  :| "email"    :> ST :>: "email address (must also be unique)"
  :| "password" :> ST :>: "password (not encrypted)"


defaultCfg :: Tagged (NoDesc Cfg) (ToConfig (NoDesc Cfg) Identity)
defaultCfg = Tagged $
     Identity (Identity 8001 :| Identity "localhost" :| Identity (Just "expose"))
  :| Identity (Identity 8002 :| Identity "localhost" :| Identity Nothing)
  :| Identity [u1, u2]
  where
    u1 = Identity "ralf" :| Identity "ralf@localhost" :| Identity "gandalf"
    u2 = Identity "claudi" :| Identity "claudi@remotehost" :| Identity "also_gandalf"


main :: IO ()
main = do
    sources <- sequence
        [ ConfigFileYaml <$> SBS.readFile "examples/config.yaml"
--        , ShellEnv       <$> getEnvironment
--        , CommandLine    <$> getArgs
        ]

    -- putStrLn $ ppShow sources

    ST.putStrLn $ docs (Proxy :: Proxy Cfg)

    let dump cfg = do
            putStrLn $ ppShow cfg
            putStrLn . cs . renderConfigFile $ cfg

    dump defaultCfg

    case configify sources :: Result (NoDesc Cfg) of
        Left e -> print e
        Right cfg -> dump cfg


{-
            putStrLn "accessing config values:"
            print $ (cfg >. (Proxy :: Proxy '["bla"]))
            print $ (cfg >. (Proxy :: Proxy '["blu", "lii"]))
            print $ (>. (Proxy :: Proxy '["go"])) <$> (cfg >. (Proxy :: Proxy '["uGH"]))
-}
