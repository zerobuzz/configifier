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

import Control.Applicative ((<$>))
import Data.String.Conversions (ST, cs)
import Data.Typeable (Proxy(Proxy))
import System.Environment (getEnvironment, getArgs)
import Text.Show.Pretty (ppShow)

import qualified Data.ByteString as SBS
import qualified Data.Text.IO as ST

import Data.Configifier


-- * an interesting example

type Cfg = NoDesc CfgDesc
type CfgDesc = ToConfigCode Cfg'

type Cfg' =
            "frontend"      :> ServerCfg :>: "descr"
  :- Maybe ("backend"       :> ServerCfg)
  :-        "default_users" :> [UserCfg] :>: "list of users that are created on start if database is empty"

type ServerCfg =
            "bind_port"   :> Int
  :-        "bind_host"   :> ST
  :- Maybe ("expose_host" :> ST)

type UserCfg =
            "name"     :> ST :>: "user name (must be unique)"
  :-        "email"    :> ST :>: "email address (must also be unique)"
  :-        "password" :> ST :>: "password (not encrypted)"


defaultCfg :: Tagged Cfg
defaultCfg = Tagged $
     Id (Id 8001 :- Id "localhost" :- JustO (Id "expose"))
  :- JustO (Id (Id 8002 :- Id "localhost" :- NothingO))
  :- Id [u1, u2]
  where
    u1 = Id "ralf" :- Id "ralf@localhost" :- Id "gandalf"
    u2 = Id "claudi" :- Id "claudi@remotehost" :- Id "also_gandalf"


main :: IO ()
main = do
    sources <- sequence
        [ ConfigFileYaml <$> SBS.readFile "examples/config.yaml"
        , ShellEnv       <$> getEnvironment
        , CommandLine    <$> getArgs
        ]

    ST.putStrLn $ docs (Proxy :: Proxy CfgDesc)

    let dump cfg = do
            putStrLn $ ppShow cfg
            putStrLn . cs . renderConfigFile $ cfg

    dump defaultCfg

    case configify sources :: Result Cfg of
        Left e -> print e
        Right cfg -> do
            dump cfg

            putStrLn "accessing config values:"
            print $ cfg >>. (Proxy :: Proxy '["frontend"])
            print $ cfg >>. (Proxy :: Proxy '["frontend", "expose_host"])
