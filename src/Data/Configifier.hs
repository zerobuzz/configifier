{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

{-# LANGUAGE UndecidableInstances  #-}  -- should only be required to run 'HasParseCommandLine'; remove later!

{-# OPTIONS  #-}

module Data.Configifier
where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Exception (assert)
import Control.Monad.Error.Class (catchError)
import Data.Aeson (ToJSON, FromJSON, Value(Object, Null), object, toJSON)
import Data.CaseInsensitive (mk)
import Data.Char (toUpper)
import Data.Function (on)
import Data.List (nubBy, intercalate, sort)
import Data.Maybe (catMaybes)
import Data.String.Conversions (ST, SBS, cs, (<>))
import Data.Typeable (Typeable, Proxy(Proxy), typeOf)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Safe (readMay)

-- (FIXME: can i replace aeson entirely with yaml?  right now, the mix
-- of use of both is rather chaotic.)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Data.Yaml as Yaml
import qualified Text.Regex.Easy as Regex


-- * servant-style type combinators

-- | the equivalent of record field selectors.
data (path :: k) :> v = Proxy path :> v
  deriving (Eq, Ord, Show, Typeable)
infixr 9 :>

-- | descriptive strings for documentation.  (the way we use this is
-- still a little awkward.  use tuple of name string and descr string?
-- or a type class "path" with a type family that translates both @""
-- :>: ""@ and @""@ to @""@?)
data (path :: k) :>: descr = Proxy path :>: descr
  deriving (Eq, Ord, Show, Typeable)
infixr 9 :>:

-- | @cons@ for record fields.
data a :| b = a :| b
  deriving (Eq, Ord, Show, Typeable)
infixr 6 :|


-- * sources

data Source =
      ConfigFile SBS
    | ShellEnv [(String, String)]
    | CommandLine [String]
  deriving (Eq, Ord, Show, Typeable)

data ConfigFile
data ShellEnv
data CommandLine

data Error =
      InvalidJSON String
    | ShellEnvNoParse (String, String)
    | ShellEnvSegmentNotFound String
    | CommandLinePrimitiveParseError String
    | CommandLinePrimitiveOther Error
  deriving (Eq, Ord, Show, Typeable)

configify :: forall cfg .
      ( FromJSON cfg
      , HasParseConfigFile cfg
      , HasParseShellEnv cfg
      , HasParseCommandLine cfg
      ) => [Source] -> Either Error cfg
configify sources = sequence (_get <$> sources) >>= _parse . merge
  where
    proxy = Proxy :: Proxy cfg

    _get :: Source -> Either Error Aeson.Value
    _get (ConfigFile sbs)   = parseConfigFile proxy sbs
    _get (ShellEnv env)     = parseShellEnv proxy env
    _get (CommandLine args) = parseCommandLine proxy args

    _parse :: Aeson.Value -> Either Error cfg
    _parse = either (Left . InvalidJSON) (Right) . Aeson.parseEither Aeson.parseJSON


-- * json / yaml

class HasParseConfigFile cfg where
    parseConfigFile :: Proxy cfg -> SBS -> Either Error Aeson.Value

instance HasParseConfigFile cfg where
    parseConfigFile Proxy sbs = either (Left . InvalidJSON) (Right) (Yaml.decodeEither sbs)

instance (KnownSymbol path, FromJSON v) => FromJSON (path :> v) where
    parseJSON = Aeson.withObject "config object" $ \ m ->
        let proxy = Proxy :: Proxy path
            key = cs $ symbolVal proxy
        in (proxy :>) <$> m Aeson..: key

instance (FromJSON o1, FromJSON o2)
        => FromJSON (o1 :| o2) where
    parseJSON value = (:|) <$> (Aeson.parseJSON value :: Aeson.Parser o1)
                           <*> (Aeson.parseJSON value :: Aeson.Parser o2)

instance (KnownSymbol path, KnownSymbol descr, FromJSON v) => FromJSON (path :>: descr :> v) where
    parseJSON = Aeson.withObject "config object" $ \ m ->
        let pproxy = Proxy :: Proxy path
            dproxy = Proxy :: Proxy descr
            key = cs $ symbolVal pproxy
        in (pproxy :>:) . (dproxy :>) <$> m Aeson..: key

instance (KnownSymbol path, ToJSON v) => ToJSON (path :> v) where
    toJSON (path :> v) = object [cs (symbolVal path) Aeson..= toJSON v]

instance (ToJSON o1, ToJSON o2) => ToJSON (o1 :| o2) where
    toJSON (o1 :| o2) = toJSON o1 <<>> toJSON o2

instance (KnownSymbol path, KnownSymbol descr, ToJSON v) => ToJSON (path :>: descr :> v) where
    toJSON (path :>: _ :> v) = toJSON (path :> v)


-- * shell env.

type Env = [(String, String)]

class HasParseShellEnv a where
    parseShellEnv :: Proxy a -> Env -> Either Error Aeson.Value

class HasParseShellEnv' a where
    parseShellEnv' :: Proxy a -> Env -> Either Error Aeson.Pair

instance HasParseShellEnv Int where
    parseShellEnv Proxy [("", s)] = Aeson.Number <$> _catch (readMay s)
      where
        _catch = maybe (Left $ ShellEnvNoParse ("Int", s)) Right

instance HasParseShellEnv Bool where
    parseShellEnv Proxy [("", s)] = Aeson.Bool <$> _catch (readMay s)
      where
        _catch = maybe (Left $ ShellEnvNoParse ("Bool", s)) Right

instance HasParseShellEnv ST where
    parseShellEnv Proxy [("", s)] = Right . Aeson.String $ cs s

-- | since shell env is not ideally suitable for providing
-- arbitrary-length lists of sub-configs, we cheat: if a value is fed
-- into a place where a list of values is expected, a singleton list
-- is constructed implicitly.
instance HasParseShellEnv a => HasParseShellEnv [a] where
    parseShellEnv Proxy = fmap (Aeson.Array . Vector.fromList . (:[])) . parseShellEnv (Proxy :: Proxy a)

-- | the sub-object structure of the config file is represented by '_'
-- in the shell variable names.  (i think it's still ok to have '_' in
-- your config variable names instead of caml case; this parser first
-- chops off matching names, then worries about trailing '_'.)
instance (KnownSymbol path, HasParseShellEnv v) => HasParseShellEnv (path :> v) where
    parseShellEnv Proxy env = do
        let key = symbolVal (Proxy :: Proxy path)
            env' = catMaybes $ _crop <$> env

            _crop :: (String, String) -> Maybe (String, String)
            _crop (k, v) = case splitAt (length key) k of
                (key', s@"")        | mk key == mk key' -> Just (s, v)
                (key', '_':s@(_:_)) | mk key == mk key' -> Just (s, v)
                _                                       -> Nothing

        if null env'
            then Left $ ShellEnvSegmentNotFound key
            else do
                val :: Aeson.Value <- parseShellEnv (Proxy :: Proxy v) env'
                return $ object [cs key Aeson..= val]

instance (KnownSymbol path, HasParseShellEnv v, HasParseShellEnv o) => HasParseShellEnv (path :> v :| o) where
    parseShellEnv Proxy env = mergeAndCatch
             [ parseShellEnv (Proxy :: Proxy o)           env
             , parseShellEnv (Proxy :: Proxy (path :> v)) env
             ]

instance (KnownSymbol path, KnownSymbol descr, HasParseShellEnv v) => HasParseShellEnv (path :>: descr :> v) where
    parseShellEnv Proxy = parseShellEnv (Proxy :: Proxy (path :> v))

instance (KnownSymbol path, KnownSymbol descr, HasParseShellEnv v, HasParseShellEnv o) => HasParseShellEnv (path :>: descr :> v :| o) where
    parseShellEnv Proxy = parseShellEnv (Proxy :: Proxy (path :> v :| o))


-- * cli

type Args = [String]

class HasParseCommandLine cfg where
    parseCommandLine :: Proxy cfg -> [String] -> Either Error Aeson.Value

instance (HasParseShellEnv cfg) => HasParseCommandLine cfg where
    parseCommandLine = primitiveParseCommandLine


-- | Very basic fist approach: read @/--(key)(=|\s+)(value)/@;
-- construct shell env from keys and names, and use 'parseShellEnv' on
-- the command line.  If it doesn't like the syntax used in the
-- command line, it will crash.  I hope for this to get much fancier
-- in the future.
primitiveParseCommandLine :: (HasParseShellEnv cfg) => Proxy cfg -> [String] -> Either Error Aeson.Value
primitiveParseCommandLine proxy args =
      convertParseError (lastWins <$> parseArgs args)
          >>= convertShellEnvError . parseShellEnv proxy
  where
    convertParseError    = either (Left . CommandLinePrimitiveParseError) Right
    convertShellEnvError = either (Left . CommandLinePrimitiveOther) Right
    lastWins             = reverse . nubBy ((==) `on` fst) . reverse

parseArgs :: Args -> Either String Env
parseArgs [] = Right []
parseArgs (h:[]) = parseArgsWithEqSign h
parseArgs (h:h':t) = ((++) <$> parseArgsWithEqSign h   <*> parseArgs (h':t))
                 <|> ((++) <$> parseArgsWithSpace h h' <*> parseArgs t)

parseArgsWithEqSign :: String -> Either String Env
parseArgsWithEqSign s = case cs s Regex.=~- "^--([^=]+)=(.*)$" of
    [_, k, v] -> Right [(cs k, cs v)]
    bad -> Left $ "could not parse last arg: " ++ show (s, bad)

parseArgsWithSpace :: String -> String -> Either String Env
parseArgsWithSpace s v = case cs s Regex.=~- "^--([^=]+)$" of
    [_, k] -> Right [(cs k, cs v)]
    bad -> Left $ "could not parse long-arg with value: " ++ show (s, v, bad)


-- * access config types

-- | Using '(>.)' works, but is not very convenient: We need a lot of
-- type signatures to help the compiler, and a lot of proxies, which
-- are a lot more verbose than lenses.  Perhaps we need to
-- auto-generate lenses after all, in the spirit of 'makeLenses'.
class HasSelector a sel v where
    (>.) :: a -> Proxy sel -> v

instance (KnownSymbol path, KnownSymbol sel, path ~ sel)
        => HasSelector (path :> v) sel v where
    (>.) (_ :> v) Proxy = v

instance (KnownSymbol path, KnownSymbol sel, path ~ sel)
        => HasSelector (path :> v :| o) sel v where
    (>.) (_ :> v :| _) Proxy = v

instance (HasSelector o sel v)
        => HasSelector (n :| o) sel v where
    (>.) (_ :| o) = (>.) o

instance (KnownSymbol path, KnownSymbol descr, KnownSymbol sel, path ~ sel)
        => HasSelector (path :>: descr :> v) sel v where
    (>.) (_ :>: _ :> v) Proxy = v

instance (KnownSymbol path, KnownSymbol descr, KnownSymbol sel, path ~ sel)
        => HasSelector (path :>: descr :> v :| o) sel v where
    (>.) (_ :>: _ :> v :| _) Proxy = v


-- * merge configs

-- | Merge two json trees such that the latter overwrites nodes in the
-- former.  'Null' is considered as non-existing.  Otherwise, right
-- values overwrite left values.
(<<>>) :: Aeson.Value -> Aeson.Value -> Aeson.Value
(<<>>) (Object m) (Object m') = object $ f <$> ks
  where
    ks :: [ST]
    ks = Set.toList . Set.fromList $ HashMap.keys m ++ HashMap.keys m'

    f :: ST -> Aeson.Pair
    f k = k Aeson..=
        case (k `HashMap.lookup` m, k `HashMap.lookup` m') of
            (Just v,  Just v') -> v <<>> v'
            (Nothing, Just v') -> v'
            (Just v,  Nothing) -> v
            (Nothing, Nothing) -> assert False $ error "internal error in (<<>>)"

(<<>>) v Null = v
(<<>>) _ v'   = v'

merge :: [Aeson.Value] -> Aeson.Value
merge = foldl (<<>>) Aeson.Null

mergeAndCatch :: [Either Error Aeson.Value] -> Either Error Aeson.Value
mergeAndCatch = foldl (\ ev ev' -> (<<>>) <$> c'tcha ev <*> c'tcha ev') (Right Null)
  where
    -- (this name is a sound from the samurai jack title tune.  it
    -- has nothing to do with this, but it sounds nice.  go watch
    -- samurai jack!)
    c'tcha = (`catchError` \ _ -> return Null)


-- * docs.

docs :: ( HasToDoc a
        , HasRenderDoc ConfigFile
        , HasRenderDoc ShellEnv
        , HasRenderDoc CommandLine
        ) => Proxy a -> ST
docs proxy = renderDoc (Proxy :: Proxy ConfigFile)  (toDoc proxy)
          <> renderDoc (Proxy :: Proxy ShellEnv)    (toDoc proxy)
          <> renderDoc (Proxy :: Proxy CommandLine) (toDoc proxy)

data Doc =
    DocDict [(String, Maybe String, Doc)]
  | DocList Doc
  | DocBase String
  deriving (Eq, Ord, Show, Read, Typeable)

concatDoc :: Doc -> Doc -> Doc
concatDoc (DocDict xs) (DocDict ys) = DocDict . sort $ xs ++ ys
concatDoc bad bad' = error $ "concatDoc: " ++ show (bad, bad')


class HasToDoc a where
    toDoc :: Proxy a -> Doc

_makeDocPair :: (KnownSymbol path, KnownSymbol descr, HasToDoc v)
      => Proxy path -> Maybe (Proxy descr) -> Proxy v -> Doc
_makeDocPair pathProxy descrProxy vProxy = DocDict [(symbolVal pathProxy, symbolVal <$> descrProxy, toDoc vProxy)]

instance (KnownSymbol path, HasToDoc v) => HasToDoc (path :> v) where
    toDoc Proxy = _makeDocPair (Proxy :: Proxy path) (Nothing :: Maybe (Proxy path)) (Proxy :: Proxy v)

instance (KnownSymbol path, KnownSymbol descr, HasToDoc v) =>  HasToDoc (path :>: descr :> v) where
    toDoc Proxy = _makeDocPair (Proxy :: Proxy path) (Just (Proxy :: Proxy descr)) (Proxy :: Proxy v)

instance (HasToDoc o1, HasToDoc o2) => HasToDoc (o1 :| o2) where
    toDoc Proxy = toDoc (Proxy :: Proxy o1) `concatDoc` toDoc (Proxy :: Proxy o2)

instance HasToDoc a => HasToDoc [a] where
    toDoc Proxy = DocList . toDoc $ (Proxy :: Proxy a)

instance HasToDoc ST where
    toDoc Proxy = DocBase "string"

instance HasToDoc Int where
    toDoc Proxy = DocBase "number"

instance HasToDoc Bool where
    toDoc Proxy = DocBase "boolean"


class HasRenderDoc t where
    renderDoc :: Proxy t -> Doc -> ST

instance HasRenderDoc ConfigFile where
    renderDoc Proxy doc = cs . unlines $
        "" :
        "Config File" :
        "-----------" :
        "" :
        f doc ++
        "" :
        []
      where
        f :: Doc -> [String]
        f (DocDict cs) = concat $ map g cs
        f (DocList doc) = indent "- " $ f doc
        f (DocBase base) = [base]

        g :: (String, Maybe String, Doc) -> [String]
        g (key, Just mDescr, subdoc) = ("# " <> mDescr) : (key <> ":") : indent "  " (f subdoc)
        g (key, Nothing,     subdoc) =                    (key <> ":") : indent "  " (f subdoc)

        indent :: String -> [String] -> [String]
        indent start = lines . (start <>) . intercalate "\n  "


instance HasRenderDoc ShellEnv where
    renderDoc Proxy doc = cs . unlines $
        "" :
        "Shell Environment Variables" :
        "---------------------------" :
        "" :
        (f [] doc) ++
        "" :
        []
      where
        f :: [(String, Maybe String)] -> Doc -> [String]
        f acc (DocDict xs) = concat $ map (g acc) xs
        f acc (DocList doc) = f acc doc
        f (reverse -> acc) (DocBase base) =
                shellvar :
                ("    type: " ++ base) :
                (let xs = catMaybes (mkd <$> acc) in
                  if null xs then [] else "    documented components:" : xs) ++
                "" :
                []
          where
            shellvar :: String
            shellvar = map toUpper . intercalate "_" . map fst $ acc

            mkd :: (String, Maybe String) -> Maybe String
            mkd (key, Nothing) = Nothing
            mkd (key, Just descr) = Just $ "        " ++ (toUpper <$> key) ++ ": " ++ descr

        g :: [(String, Maybe String )] -> (String, Maybe String, Doc) -> [String]
        g acc (key, descr, subdoc) = f ((key, descr) : acc) subdoc

instance HasRenderDoc CommandLine where
    renderDoc Proxy doc = cs . unlines $
        "" :
        "Command Line Arguments" :
        "----------------------" :
        "" :
        f doc ++
        "" :
        []
      where
        f _ = []
