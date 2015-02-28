{-# LANGUAGE BangPatterns                             #-}
{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE ExistentialQuantification                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE InstanceSigs                             #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE NoImplicitPrelude                        #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE PackageImports                           #-}
{-# LANGUAGE PatternGuards                            #-}
{-# LANGUAGE QuasiQuotes                              #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE StandaloneDeriving                       #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeSynonymInstances                     #-}
{-# LANGUAGE ViewPatterns                             #-}

{-# OPTIONS  #-}

module Main
where


-- yaml
-- generics
-- lenses


data Config =
    Config
      { _bla :: Int
      , _blu :: SubConfig
      }

data SubConfig =
    Subconfig
      { _lii :: Bool
      }


class (Functor m, Applicative m, MonadIO m, MonadError m) => EMonoid m a where
  mzero :: m a
  mappend :: a -> a -> m a

instance EMonoid Config where
  emzero = Config <$> 3 <*> emzero
  emappend (Config bla blu) (Config bla' blu') = Config bla' <$> (blu <|> blu')

  -- (it appears that only subconfigs that instantiate EMonoid can
  -- leave configuration for later sources.

instance EMonoid SubConfig where
  mzero = throw "no sub-config!"
  emappend a b = pure b


data Source = CommandLine | ShellEnv | ConfigFile


class EMonoid m a => HasCommandLine m a where
  parseCommandLine :: m a

class EMonoid m a => HasShellEnv m a where
  parseShellEnv :: m a

class EMonoid m a => HasConfigFile m a where
  parseConfigFile :: m a  -- (where does the FilePath come from?)


data CommandLine deriving Typeable
data ShellEnv deriving Typeable
data ConfigFile deriving Typeable


class HasConfigify '[source] m a where
  configify :: m a

instance (HasCommandLine m a) => HasConfigify '[CommandLine, ...] m a where
  configify = ...

...


-- | as yaml to stdout, or as json if desired.  there is also a
-- sub-config that provides that in your application's cli.
writeDefaultConfig :: ToJSON a => IO ()
writeDefaultConfig = _
