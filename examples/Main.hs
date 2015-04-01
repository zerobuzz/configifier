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
import Data.String.Conversions (ST, cs, (<>))
import Data.Typeable (Proxy(Proxy))
import System.Environment (getEnvironment, getArgs)
import Text.Show.Pretty (ppShow)

import qualified Data.ByteString as SBS
import qualified Data.Text.IO as ST

import Data.Configifier


-- * an interesting example

type Cfg = ToConfigCode Cfg'
type Cfg' =
             "frontend"      :> ServerCfg' :>: "descr"
  :*> Maybe ("backend"       :> ServerCfg')
  :*>        "default_users" :> [UserCfg'] :>: "list of users that are created on start if database is empty"

type ServerCfg = ToConfigCode ServerCfg'
type ServerCfg' =
             "bind_port"   :> Int
  :*>        "bind_host"   :> ST
  :*> Maybe ("expose_host" :> ST)

type UserCfg = ToConfigCode UserCfg'
type UserCfg' =
             "name"     :> ST :>: "user name (must be unique)"
  :*>        "email"    :> ST :>: "email address (must also be unique)"
  :*>        "password" :> ST :>: "password (not encrypted)"


defaultCfg :: Tagged Cfg
defaultCfg = Tagged $
      Id (Id 8001 :*> Id "localhost" :*> JustO (Id "expose"))
  :*> JustO (Id (Id 8002 :*> Id "localhost" :*> NothingO))
  :*> Id [u1, u2]
  where
    u1 = Id "ralf" :*> Id "ralf@localhost" :*> Id "gandalf"
    u2 = Id "claudi" :*> Id "claudi@remotehost" :*> Id "also_gandalf"


main :: IO ()
main = do
    sources <- sequence
        [ YamlString  <$> SBS.readFile "examples/config.yaml"
        , ShellEnv    <$> getEnvironment
        , CommandLine <$> getArgs
        ]

    ST.putStrLn $ docs (Proxy :: Proxy Cfg) <> "\n\n"

    let dump cfg = do
            putStrLn $ ppShow cfg
            putStrLn . cs . renderConfigFile $ cfg

    dump defaultCfg

    cfg :: Tagged Cfg <- configify sources
    dump cfg

    putStrLn "accessing config values:"
    print $ (Tagged (cfg >>. (Proxy :: Proxy '["frontend"])) :: Tagged ServerCfg)
    print $ cfg >>. (Proxy :: Proxy '["frontend", "expose_host"])
    print $ cfg >>. (Proxy :: Proxy '["frontend", "bind_port"])


{-

Example session:

$ configifier-example
[...]
accessing config values:
(Tagged Id 8001 :*> (Id "localhost" :*> JustO (Id "fost")))
Just "fost"
8001

$ FRONTEND_BIND_PORT=31 configifier-example
[...]
accessing config values:
(Tagged Id 31 :*> (Id "localhost" :*> JustO (Id "fost")))
Just "fost"
31

$ FRONTEND_BIND_PORT=31 configifier-example --frontend-bind-port 15
[...]
accessing config values:
(Tagged Id 15 :*> (Id "localhost" :*> JustO (Id "fost")))
Just "fost"
15

$ configifier-example --frontend-expose-host "false"
[...]
configifier-example: CommandLinePrimitiveOtherError (ShellEnvNoParse {shellEnvNoParseType = "Text", shellEnvNoParseValue = "false", shellEnvNoParseMsg = "when expecting a Text, encountered Boolean instead"})

$ configifier-example --frontend-expose-host "\"false\""
[...]
accessing config values:
(Tagged Id 8001 :*> (Id "localhost" :*> JustO (Id "false")))
Just "false"
8001

$ configifier-example --config examples/config2.yaml
[...]
backend:
  bind_host: arrr
  bind_port: 281
[...]

-}
