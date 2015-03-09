{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
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
import Control.Exception (Exception, assert)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Identity
import Data.Aeson (ToJSON, FromJSON, Value(Object, Null), object, toJSON)
import Data.CaseInsensitive (mk)
import Data.Char (toUpper)
import Data.Either.Combinators (mapLeft)
import Data.Function (on)
import Data.List (nubBy, intercalate, sort)
import Data.Maybe (catMaybes)
import Data.String.Conversions (ST, SBS, cs, (<>))
import Data.Typeable (Typeable, Proxy(Proxy), typeOf)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
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


-- * type combinators

-- | Construction of config records (@cons@ for record fields).
data a :| b = a :| b
  deriving (Eq, Ord, Show, Typeable)
infixr 6 :|

-- | Construction of config record fields.
data (s :: Symbol) :> (t :: *)
infixr 9 :>

-- | Add descriptive text to record field for documentation.
data a :>: (s :: Symbol)
infixr 8 :>:


-- * ...

data ConfigCode a =
      Choice (ConfigCode a) (ConfigCode a)
    | Descr  (ConfigCode a) Symbol
    | Label  Symbol (ConfigCode a)
    | List   (ConfigCode a)
    | Type   a

infixr 6 `Choice`

-- | Map user-provided config type to 'ConfigCode' types.
type family ToConfigCode (x :: *) :: ConfigCode * where
    ToConfigCode (a :| b)       = Choice (ToConfigCode a) (ToConfigCode b)
    ToConfigCode (s :> t :>: s) = Descr (ToConfigCode (s :> t)) s
    ToConfigCode (s :> t)       = Label s (ToConfigCode t)
    ToConfigCode [a]            = List (ToConfigCode a)
    ToConfigCode a              = Type a

-- | Map 'ConfgCode' types to the types of config values.
type family ToConfig (c :: ConfigCode *) (f :: * -> *) :: * where
    ToConfig (Choice a b) f = ToConfig a f :| ToConfig b f
    ToConfig (Descr a d)  f = ToConfig a f
    ToConfig (Label s t)  f = f (ToConfig t f)
    ToConfig (List c)     f = [ToConfig c f]
    ToConfig (Type a)     f = a


-- * sources

data Source =
      ConfigFileYaml SBS
    | ShellEnv [(String, String)]
    | CommandLine [String]
  deriving (Eq, Ord, Show, Typeable)

data ConfigFile
data ShellEnv
data CommandLine

data Error =
      InvalidYaml String
    | ShellEnvNoParse (String, String, String)
    | ShellEnvSegmentNotFound String
    | CommandLinePrimitiveParseError String
    | CommandLinePrimitiveOtherError Error
    | FreezeIncomplete
  deriving (Eq, Ord, Show, Typeable)

instance Exception Error

configify :: forall cfg val mval .
      ( val ~ ToConfig cfg Identity
      , mval ~ ToConfig cfg Maybe
      , Merge cfg
      , HasParseConfigFile cfg
--      , HasParseShellEnv cfg
--      , HasParseCommandLine cfg
      ) => Proxy cfg -> [Source] -> Either Error val
configify proxy [] = error "configify: no sources!"
configify proxy sources = sequence (get <$> sources) >>= merge proxy
  where
    get :: Source -> Either Error mval
    get (ConfigFileYaml sbs) = parseConfigFile proxy sbs
--    get (ShellEnv env)       = parseShellEnv proxy env
--    get (CommandLine args)   = parseCommandLine proxy args



class HasParseConfigFile cfg where
    parseConfigFile :: Proxy cfg -> SBS -> Either Error (ToConfig cfg Maybe)



{-

-- * json / yaml

class HasParseConfigFile cfg where
    parseConfigFile :: Proxy cfg -> SBS -> Either Error Aeson.Value

instance HasParseConfigFile cfg where
    parseConfigFile Proxy sbs = mapLeft InvalidYaml $ Yaml.decodeEither sbs

instance (KnownSymbol path, FromJSON v) => FromJSON (path :> v) where
    parseJSON = Aeson.withObject "config object" $ \ m ->
        let proxy = Proxy :: Proxy path
            key = cs $ symbolVal proxy
        in L <$> m Aeson..: key

instance (FromJSON o1, FromJSON o2)
        => FromJSON (o1 :| o2) where
    parseJSON value = (:|) <$> (Aeson.parseJSON value :: Aeson.Parser o1)
                           <*> (Aeson.parseJSON value :: Aeson.Parser o2)

instance (KnownSymbol path, KnownSymbol descr, FromJSON v) => FromJSON (path :> v :>: descr) where
    parseJSON = Aeson.withObject "config object" $ \ m ->
        let key = cs $ symbolVal (Proxy :: Proxy path)
        in D . L <$> m Aeson..: key

instance (KnownSymbol path, ToJSON v) => ToJSON (path :> v) where
    toJSON (L v) = object [cs (symbolVal (Proxy :: Proxy path)) Aeson..= toJSON v]

instance (ToJSON o1, ToJSON o2) => ToJSON (o1 :| o2) where
    toJSON (o1 :| o2) = toJSON o1 <<>> toJSON o2

instance (KnownSymbol path, KnownSymbol descr, ToJSON v) => ToJSON (path :> v :>: descr) where
    toJSON (D l) = toJSON l

-- | Basic parts of the config structure parse as yaml in all sources:
-- In order to pass a value of type '[(Int, Int)]' via shell env with
-- @CFG_FIELD="[(3, 4), (1, 2)]"@, wrap the 'field' part of the config
-- structure in 'Basic':
--
-- > type Cfg = "field" :> Basic [(Int, Int)]
newtype Basic a = Basic a
  deriving (Eq, Ord, Show, Read, Typeable)

instance (ToJSON v) => HasParseConfigFile (Basic v) where
    parseConfigFile Proxy sbs = parseConfigFile (Proxy :: Proxy v) sbs


-- * shell env.

type Env = [(String, String)]

class HasParseShellEnv a where
    parseShellEnv :: Proxy a -> Env -> Either Error Aeson.Value

-- | The sub-object structure of the config file is represented by '_'
-- in the shell variable names.  (It is still ok to have '_' in your
-- config path names instead of caml case; this parser chops off
-- complete matching names, whether they contain '_' or not, and only
-- then worries about trailing '_'.)
instance (KnownSymbol path, HasParseShellEnv v) => HasParseShellEnv (path :> v) where
    parseShellEnv Proxy env = do
        let key = symbolVal (Proxy :: Proxy path)
            env' = catMaybes $ crop <$> env

            crop :: (String, String) -> Maybe (String, String)
            crop (k, v) = case splitAt (length key) k of
                (key', s@"")        | mk key == mk key' -> Just (s, v)
                (key', '_':s@(_:_)) | mk key == mk key' -> Just (s, v)
                _                                       -> Nothing

        if null env'
            then Left $ ShellEnvSegmentNotFound key
            else do
                val :: Aeson.Value <- parseShellEnv (Proxy :: Proxy v) env'
                return $ object [cs key Aeson..= val]

instance (KnownSymbol path, HasParseShellEnv v, HasParseShellEnv o)
        => HasParseShellEnv (path :> v :| o) where
    parseShellEnv Proxy env = merge <$> sequence
             [ parseShellEnv (Proxy :: Proxy (path :> v)) env
             , parseShellEnv (Proxy :: Proxy o)           env
             ]

instance (KnownSymbol path, KnownSymbol descr, HasParseShellEnv v)
        => HasParseShellEnv (path :> v :>: descr) where
    parseShellEnv Proxy = parseShellEnv (Proxy :: Proxy (path :> v))

instance (KnownSymbol path, KnownSymbol descr, HasParseShellEnv v, HasParseShellEnv o)
        => HasParseShellEnv (path :> v :>: descr :| o) where
    parseShellEnv Proxy = parseShellEnv (Proxy :: Proxy (path :> v :| o))

instance (Typeable a, ToJSON a, FromJSON a) => HasParseShellEnv (Basic a) where
    parseShellEnv Proxy = parseYamlToShellEnv (Proxy :: Proxy a)

instance HasParseShellEnv Int where
    parseShellEnv = parseYamlToShellEnv

instance HasParseShellEnv Bool where
    parseShellEnv = parseYamlToShellEnv

-- | (String values should not need to be quoted, so they are not
-- handled by 'Yaml.decode'.)
instance HasParseShellEnv ST where
    parseShellEnv Proxy [("", s)] = Right . Aeson.String $ cs s

-- | Pairs are treated as 'Basic' implicitly (without being
-- newtype-wrapped).
instance (Typeable a, ToJSON a, FromJSON a, Typeable b, ToJSON b, FromJSON b)
        => HasParseShellEnv (a, b) where
    parseShellEnv = parseYamlToShellEnv

-- | 'Maybe' values translate to either 'Aeson.Null' or the plain
-- value (no 'Just' constructor).
instance (Typeable a, FromJSON a, ToJSON a, HasParseShellEnv a)
        => HasParseShellEnv (Maybe a) where
    parseShellEnv = parseYamlToShellEnv

-- | Lists are treated as 'Basic' implicitly (without being
-- newtype-wrapped).
instance HasParseShellEnv a
        => HasParseShellEnv [a] where
    parseShellEnv Proxy = fmap (Aeson.Array . Vector.singleton) . parseShellEnv (Proxy :: Proxy a)


parseYamlToShellEnv :: forall a . (Typeable a, FromJSON a, ToJSON a)
        => Proxy a -> Env -> Either Error Aeson.Value
parseYamlToShellEnv Proxy [("", s)] = case Yaml.decodeEither $ cs s :: Either String a of
    Right a -> Right $ Aeson.toJSON a
    Left e  -> Left $ ShellEnvNoParse (show $ typeOf (undefined :: a), s, e)


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
      mapLeft CommandLinePrimitiveParseError (lastWins <$> parseArgs args)
          >>= mapLeft CommandLinePrimitiveOtherError . parseShellEnv proxy
  where
    lastWins = reverse . nubBy ((==) `on` fst) . reverse

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


-- * accessing config values

-- ** data types

-- | Type-level lookup of a path in a configuration type.
-- A path is represented as a list of symbols.
type family Val (a :: *) (p :: [Symbol]) :: Maybe * where
  Val a         '[]       = Just a
  Val (a :| b)  (p ': ps) = OrElse (Val a (p ': ps)) (Val b (p ': ps))
  Val (a :>: s) (p ': ps) = Val a (p ': ps)
  Val (p :> t)  (p ': ps) = Val t ps
  Val a         p         = Nothing

-- | This is '<|>' on 'Maybe' lifted to the type level.
type family OrElse (x :: Maybe k) (y :: Maybe k) :: Maybe k where
  OrElse (Just x) y = Just x
  OrElse Nothing  y = y

-- | A 'CMaybe' is a static version of 'Maybe', i.e., we know at
-- compile time whether we have 'Just' or 'Nothing'.
data CMaybe (a :: Maybe *) where
  CNothing :: CMaybe Nothing
  CJust    :: a -> CMaybe (Just a)

-- | This is a version of '<|>' on 'Maybe' for 'CMaybe'.
combine :: CMaybe a -> CMaybe b -> CMaybe (OrElse a b)
combine (CJust x) _ = CJust x
combine CNothing  y = y


-- ** exposed interface

-- | This is a wrapper around 'sel' that hides the interal use of
-- 'CMaybe'.  As we expect, this version will just cause a type error
-- if it is applied to an illegal path.
(>.) :: forall a p r . (Sel a p, ValE a p ~ Done r) => a -> Proxy p -> r
(>.) cfg p = case sel cfg p of
  CJust x  -> x
  _        -> error "inaccessible"

-- | FIXME: is it possible to remove 'CMaybe' from the signature and
-- return @Val a p@ instead?
class Sel a p where
  sel :: a -> Proxy p -> CMaybe (Val a p)


-- ** implementation of 'Sel'

instance Sel a '[] where
  sel x _ = CJust x

instance Sel a (p ': ps) => Sel (a :>: s) (p ': ps) where
  sel (D x) _ = sel x (Proxy :: Proxy (p ': ps))

instance Sel t ps => Sel (p :> t) (p ': ps) where
  sel (L x) _ = sel x (Proxy :: Proxy ps)

instance (Sel a (p ': ps), Sel b (p ': ps)) => Sel (a :| b) (p ': ps) where
  sel (x :| y) _ = combine (sel x (Proxy :: Proxy (p ': ps))) (sel y (Proxy :: Proxy (p ': ps)))

-- | We need the 'Val' constraint here because overlapping instances
-- and closed type families aren't fully compatible.  GHC won't be
-- able to recognize that we've already excluded the other cases and
-- not reduce 'Val' automatically.  But the constraint should always
-- resolve, unless we've made a mistake, and the worst outcome if we
-- did are extra type errors, not run-time errors.
instance (Val a ps ~ Nothing) => Sel a ps where
  sel _ _ = CNothing


-- ** better errors

data Exc a b = Fail a | Done b

data LookupFailed a p

type ValE (a :: *) (p :: [Symbol]) = ToExc (LookupFailed a p) (Val a p)

type family ToExc (a :: k) (x :: Maybe l) :: Exc k l where
  ToExc a Nothing  = Fail a
  ToExc a (Just x) = Done x

-}






-- * merge configs

merge :: Merge c => Proxy c -> [ToConfig c Maybe] -> Either Error (ToConfig c Identity)
merge Proxy [] = error "merge: empty list."
merge proxy (x:xs) = _freeze proxy $ foldl (_merge proxy) x xs


-- | Merge two partial configs.
class Merge c where
    _merge  :: Proxy c -> ToConfig c Maybe -> ToConfig c Maybe -> ToConfig c Maybe
    _freeze :: Proxy c -> ToConfig c Maybe -> Either Error (ToConfig c Identity)

instance (Merge a, Merge b) => Merge (Choice a b) where
    _merge Proxy (x :| y) (x' :| y') = _merge (Proxy :: Proxy a) x x' :| _merge (Proxy :: Proxy b) y y'
    _freeze Proxy (x :| y) = do
        x' <- _freeze (Proxy :: Proxy a) x
        y' <- _freeze (Proxy :: Proxy b) y
        Right $ x' :| y'

instance (Merge t) => Merge (Label s t) where
    _merge Proxy (Just x) (Just y) = Just $ _merge (Proxy :: Proxy t) x y
    _merge Proxy mx       my       = my <|> mx
    _freeze Proxy (Just x) = Identity <$> _freeze (Proxy :: Proxy t) x
    _freeze Proxy Nothing = Left FreezeIncomplete

instance (Merge c) => Merge (List c) where
    _merge Proxy xs ys = xs ++ ys  -- FIXME: how to delete / overwrite previously configured list values?
    _freeze Proxy xs = sequence $ _freeze (Proxy :: Proxy c) <$> xs


-- * docs.

{-

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

instance (KnownSymbol path, KnownSymbol descr, HasToDoc v) =>  HasToDoc (path :> v :>: descr) where
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
        f (DocDict xs)   = concat $ map g xs
        f (DocList x)    = indent "- " $ f x
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
        f acc (DocList x) = f acc x
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
            mkd (_,   Nothing)    = Nothing
            mkd (key, Just descr) = Just $ "        " ++ (toUpper <$> key) ++ ": " ++ descr

        g :: [(String, Maybe String )] -> (String, Maybe String, Doc) -> [String]
        g acc (key, descr, subdoc) = f ((key, descr) : acc) subdoc

instance HasRenderDoc CommandLine where
    renderDoc Proxy _ = cs . unlines $
        "" :
        "Command Line Arguments" :
        "----------------------" :
        "" :
        "See `shell environment`.  (Anything you can set with a shell" :
        "variable, you can also set with a long arg.)" :
        "" :
        []

-}
