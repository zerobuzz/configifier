{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Data.Configifier
where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Exception (Exception)
import Data.Aeson (ToJSON, FromJSON, Value(Object, Null), object, toJSON, (.=))
import Data.CaseInsensitive (mk)
import Data.Char (toUpper)
import Data.Dynamic (Dynamic, toDyn)
import Data.Either.Combinators (mapLeft)
import Data.Function (on)
import Data.List (nubBy, intercalate)
import Data.Maybe (catMaybes)
import Data.Monoid (Monoid, (<>), mempty, mappend, mconcat)
import Data.String.Conversions (ST, SBS, cs)
import Data.Typeable (Typeable, Proxy(Proxy), TypeRep, typeOf)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
import qualified Data.Yaml as Yaml
import qualified Text.Regex.Easy as Regex


-- * config types

-- | Construction of config records (@cons@ for record fields).
data a :- b = a :- b
  deriving (Eq, Ord, Show, Typeable)
infixr 6 :-

-- | Construction of config record fields.
data (s :: Symbol) :> (t :: *)
  deriving (Typeable)
infixr 9 :>

-- | Add descriptive text to record field for documentation.
data a :>: (s :: Symbol)
  deriving (Typeable)
infixr 8 :>:


data ConfigCode k =
      Record (ConfigCode k) (ConfigCode k)
    | Label  Symbol (ConfigCode k)
    | Descr  (ConfigCode k) Symbol
    | List   (ConfigCode k)
    | Option (ConfigCode k)
    | Type   k

infixr 6 `Record`


-- | Map user-provided config type to 'ConfigCode' types.
type family ToConfigCode (a :: *) :: ConfigCode * where
    ToConfigCode (a :- b)  = Record (ToConfigCode a) (ToConfigCode b)
    ToConfigCode (s :> a)  = Label s (ToConfigCode a)
    ToConfigCode (a :>: s) = Descr (ToConfigCode a) s
    ToConfigCode [a]       = List (ToConfigCode a)
    ToConfigCode (Maybe a) = Option (ToConfigCode a)
    ToConfigCode a         = Type a

-- | Filter 'Descr' constructors from 'ConfigCode'.
type family NoDesc (a :: ConfigCode *) :: ConfigCode * where
    NoDesc (Record a b) = Record (NoDesc a) (NoDesc b)
    NoDesc (Label s a)  = Label s (NoDesc a)
    NoDesc (Descr a s)  = NoDesc a
    NoDesc (List a)     = List (NoDesc a)
    NoDesc (Option a)   = Option (NoDesc a)
    NoDesc (Type a)     = Type a

-- | Map 'ConfgCode' types to the types of config values.
type family ToConfig (a :: ConfigCode *) (f :: * -> *) :: * where
    ToConfig (Record a b) f = ToConfig a f :- ToConfig b f
    ToConfig (Label s a)  f = f (ToConfig a f)
    ToConfig (List a)     f = [ToConfig a f]
    ToConfig (Option a)   f = MaybeO (ToConfig a f)
    ToConfig (Type a)     f = a

-- | 'MaybeO' is isomorphic to 'Maybe', but is only used for 'Option'
-- values.
data MaybeO a = JustO a | NothingO
  deriving (Eq, Ord, Show, Typeable)

-- | Transformers' 'Identity' is not in 'Typeable', so we roll our
-- own.  It's also less work to write.
data Id a = Id a
  deriving (Eq, Ord, Show, Typeable)


-- * sources

data Source =
      ConfigFileYaml SBS
    | ShellEnv [(String, String)]
    | CommandLine [String]
  deriving (Eq, Ord, Show, Typeable)

data ConfigFile
data ShellEnv
data CommandLine


-- * tagged values

data Tagged cfg = Tagged { fromTagged :: ToConfig cfg Id }

data TaggedM cfg = TaggedM { fromTaggedM :: ToConfig cfg Maybe }


-- deriving with type functions does not work, so we just write a lot
-- of boilerplate here.

instance (Eq (ToConfig cfg Id)) => Eq (Tagged cfg) where
    Tagged a == Tagged b = a == b

instance (Eq (ToConfig cfg Maybe)) => Eq (TaggedM cfg) where
    TaggedM a == TaggedM b = a == b

instance (Show (ToConfig cfg Id)) => Show (Tagged cfg) where
    show (Tagged x) = "(Tagged " ++ show x ++ ")"

instance (Show (ToConfig cfg Maybe)) => Show (TaggedM cfg) where
    show (TaggedM x) = "(TaggedM " ++ show x ++ ")"


-- * results and errors

type Result cfg = Either Error (Tagged cfg)

data Error =
      InvalidYaml
        { invalidYamlInput :: SBS
        , invalidYamlMsg :: String
        }
    | ShellEnvNil
    | ShellEnvNoParse
        { shellEnvNoParseType  :: String
        , shellEnvNoParseValue :: String
        , shellEnvNoParseMsg   :: String
        }
    | CommandLinePrimitiveParseError String
    | CommandLinePrimitiveOtherError Error
    | FreezeIncomplete
        { freezeIncompleteAtPath :: [String]
        }
  deriving (Eq, Ord, Show, Typeable)

instance Exception Error


-- * the main function

configify :: forall cfg tm .
      ( tm ~ TaggedM cfg
      , Show tm
      , Monoid tm
      , Freeze cfg
      , FromJSON tm
      , HasParseShellEnv cfg
      , HasParseCommandLine cfg
      , CanonicalizePartial cfg
      ) => [Source] -> Result cfg
configify sources = sequence (get <$> sources) >>= merge
  where
    get :: Source -> Either Error tm
    get (ConfigFileYaml sbs) = parseConfigFile sbs
    get (ShellEnv env)       = parseShellEnv env
    get (CommandLine args)   = parseCommandLine args


-- * yaml / json

parseConfigFile :: (FromJSON (TaggedM cfg)) => SBS -> Either Error (TaggedM cfg)
parseConfigFile sbs = mapLeft (InvalidYaml sbs) $ Yaml.decodeEither sbs

renderConfigFile :: (Freeze cfg, t ~ Tagged cfg, ToJSON (TaggedM cfg)) => t -> SBS
renderConfigFile = Yaml.encode . thaw


-- render json

-- | @instance ToJSON Record@
instance ( t1 ~ ToConfig cfg1 Maybe, ToJSON (TaggedM cfg1)
         , t2 ~ ToConfig cfg2 Maybe, ToJSON (TaggedM cfg2)
         )
        => ToJSON (TaggedM (Record cfg1 cfg2)) where
    toJSON (TaggedM (o1 :- o2)) = case ( toJSON (TaggedM o1 :: TaggedM cfg1)
                                       , toJSON (TaggedM o2 :: TaggedM cfg2)
                                       ) of
        (Object m1, Object m2) -> Object $ HashMap.union m2 m1
        (v, Null)              -> v
        (_, v')                -> v'

-- | @instance ToJSON Label@
instance ( ToJSON (TaggedM cfg)
         , KnownSymbol s
         )
        => ToJSON (TaggedM (Label s cfg)) where
    toJSON (TaggedM Nothing) = Aeson.Null
    toJSON (TaggedM (Just v)) = case toJSON (TaggedM v :: TaggedM cfg) of
        Aeson.Null -> Aeson.Null
        val        -> object [key .= val]  where key = cs $ symbolVal (Proxy :: Proxy s)

-- | @instance ToJSON List@
instance ( t ~ ToConfig cfg Maybe
         , ToJSON (TaggedM cfg)
         )
        => ToJSON (TaggedM (List cfg)) where
    toJSON (TaggedM vs) = toJSON $ (TaggedM :: t -> TaggedM cfg) <$> vs

-- | @instance ToJSON Option@
instance ( t ~ ToConfig cfg Maybe
         , ToConfig (Option cfg) Maybe ~ MaybeO t''
         , ToJSON (TaggedM cfg)
         ) => ToJSON (TaggedM (Option cfg)) where
    toJSON (TaggedM (JustO v)) = toJSON $ (TaggedM v :: TaggedM cfg)
    toJSON (TaggedM NothingO)  = Aeson.Null

-- | @instance ToJSON Type@
instance (ToJSON a) => ToJSON (TaggedM (Type a)) where
    toJSON (TaggedM v) = toJSON v


-- parse json

-- | @instance FromJSON Record@
instance (FromJSON (TaggedM cfg1), FromJSON (TaggedM cfg2)) => FromJSON (TaggedM (Record cfg1 cfg2)) where
    parseJSON json = do
        TaggedM o1 :: TaggedM cfg1 <- Aeson.parseJSON json
        TaggedM o2 :: TaggedM cfg2 <- Aeson.parseJSON json
        return . TaggedM $ o1 :- o2

-- | @instance FromJSON Label@ (tolerates unknown fields in json object.)
instance (FromJSON (TaggedM cfg), KnownSymbol s) => FromJSON (TaggedM (Label s cfg)) where
    parseJSON = Aeson.withObject "configifier object" $ \ m ->
          case HashMap.lookup key m of
            (Just json) -> TaggedM . Just . fromTaggedM <$> parseJSON' json
            Nothing     -> return $ TaggedM Nothing
        where
          key = cs $ symbolVal (Proxy :: Proxy s)
          parseJSON' :: Aeson.Value -> Aeson.Parser (TaggedM cfg) = Aeson.parseJSON

-- | @instance ParseJSON List@
instance (FromJSON (TaggedM cfg)) => FromJSON (TaggedM (List cfg)) where
    parseJSON = Aeson.withArray "configifier list" $ \ vector -> do
        vector' :: [TaggedM cfg] <- sequence $ Aeson.parseJSON <$> Vector.toList vector
        return . TaggedM . (fromTaggedM <$>) $ vector'

-- | @instance ParseJSON Option@
instance (FromJSON (TaggedM cfg)) => FromJSON (TaggedM (Option cfg)) where
    parseJSON Null = return (TaggedM NothingO :: TaggedM (Option cfg))
    parseJSON v = do
        TaggedM js :: TaggedM cfg <- Aeson.parseJSON v
        return $ (TaggedM (JustO js) :: TaggedM (Option cfg))

-- | @instance FromJSON Type@
instance (FromJSON a) => FromJSON (TaggedM (Type a)) where
    parseJSON = (TaggedM <$>) . Aeson.parseJSON


-- * shell env

type Env = [(String, String)]

class HasParseShellEnv (cfg :: ConfigCode *) where
    parseShellEnv :: Env -> Either Error (TaggedM cfg)

instance (HasParseShellEnv a, HasParseShellEnv b) => HasParseShellEnv (Record a b) where
    parseShellEnv env = do
        TaggedM x :: TaggedM a <- parseShellEnv env
        TaggedM y :: TaggedM b <- parseShellEnv env
        return . TaggedM $ x :- y

-- | The paths into the recursive structure of the config file are
-- concatenated to shell variable names with separating '_'.  (It is
-- still ok to have '_' in your config path names.  This parser chops
-- off complete matching names, whether they contain '_' or not, and
-- only then worries about trailing '_'.)
instance (KnownSymbol path, HasParseShellEnv a) => HasParseShellEnv (Label path a) where
    parseShellEnv [] = return $ TaggedM Nothing
    parseShellEnv env@(_:_) =
          case parseShellEnv env' :: Either Error (TaggedM a) of
              Right (TaggedM a) -> Right . TaggedM . Just $ a
              Left ShellEnvNil  -> Right . TaggedM $ Nothing
              Left e            -> Left e
      where
        key = symbolVal (Proxy :: Proxy path)
        env' = catMaybes $ crop <$> env

        crop :: (String, String) -> Maybe (String, String)
        crop (k, v) = case splitAt (length key) k of
            (key', s@"")        | mk key == mk key' -> Just (s, v)
            (key', '_':s@(_:_)) | mk key == mk key' -> Just (s, v)
            _                                       -> Nothing

-- | You can provide a list value via the shell environment by
-- providing a single element.  This element will be put into a list
-- implicitly.
--
-- (A more general approach that allows for yaml-encoded list values
-- in shell variables is more tricky to design, implement, and use: If
-- you have a list of sub-configs and don't want the entire sub-config
-- to be yaml-encoded, but use a longer shell variable name to go
-- further down to deeper sub-configs, there is a lot of ambiguity.
-- It may be possible to resolve that at run-time, but it's more
-- tricky.)
instance (HasParseShellEnv a) => HasParseShellEnv (List a) where
    parseShellEnv env = do
        TaggedM a :: TaggedM a <- parseShellEnv env
        return $ TaggedM [a]

instance HasParseShellEnv a => HasParseShellEnv (Option a) where
    parseShellEnv env = do
        TaggedM a :: TaggedM a <- parseShellEnv env
        return $ TaggedM (JustO a)

instance (Typeable a, FromJSON (TaggedM (Type a))) => HasParseShellEnv (Type a) where
    parseShellEnv = f
      where
        f [] = Left $ ShellEnvNil
        f (filter (null . fst) -> [("", s)]) = mapLeft renderError (Yaml.decodeEither (cs s))
          where
            renderError :: String -> Error
            renderError e = ShellEnvNoParse (show $ typeOf (undefined :: a)) s e
        f bad = error $ "instance HasParseShellEnv (Type a): inconsistent environment: " ++ show bad


-- * cli

type Args = [String]

class HasParseCommandLine cfg where
    parseCommandLine :: [String] -> Either Error (TaggedM cfg)

instance (HasParseShellEnv cfg) => HasParseCommandLine cfg where
    parseCommandLine = primitiveParseCommandLine


-- | Very basic fist approach: read @/--(key)(=|\s+)(value)/@;
-- construct shell env from keys and names, and use 'parseShellEnv' on
-- the command line.  If it doesn't like the syntax used in the
-- command line, it will crash.  I hope for this to get much fancier
-- in the future.
primitiveParseCommandLine :: (HasParseShellEnv cfg) => [String] -> Either Error (TaggedM cfg)
primitiveParseCommandLine args =
      mapLeft CommandLinePrimitiveParseError (lastWins <$> parseArgs args)
          >>= mapLeft CommandLinePrimitiveOtherError . parseShellEnv
  where
    lastWins = reverse . nubBy ((==) `on` fst) . reverse

parseArgs :: Args -> Either String Env
parseArgs [] = Right []
parseArgs (h:[]) = parseArgsWithEqSign h
parseArgs (h:h':t) = ((++) <$> parseArgsWithEqSign h   <*> parseArgs (h':t))
                 <|> ((++) <$> parseArgsWithSpace h h' <*> parseArgs t)

parseArgsWithEqSign :: String -> Either String Env
parseArgsWithEqSign s = case cs s Regex.=~- "^--([^=]+)=(.*)$" of
    [_, k, v] -> Right [(map toUpper $ cs k, cs v)]
    bad -> Left $ "could not parse last arg: " ++ show (s, bad)

parseArgsWithSpace :: String -> String -> Either String Env
parseArgsWithSpace s v = case cs s Regex.=~- "^--([^=]+)$" of
    [_, k] -> Right [(map toUpper $ cs k, cs v)]
    bad -> Left $ "could not parse long-arg with value: " ++ show (s, v, bad)


-- * accessing config values

-- | Map a 'Tagged' config value and a type-level path to the part of
-- the config value the path points to.  Trigger an informative type
-- error if path does not exist.
(>>.) :: forall cfg t ps r . (Sel cfg ps, ToValE cfg ps ~ Done r) => Tagged cfg -> Proxy ps -> r
(>>.) v p = case sel v p of
    CJust x -> x
    _       -> error "inaccessible"


-- | Map 'ConfgCode' types to the types of config values.
type family ToVal (a :: ConfigCode *) (p :: [Symbol]) :: Maybe * where
    ToVal (Record a b) ps        = OrElse (ToVal a ps) (ToVal b ps)
    ToVal (Label p a)  (p ': ps) = ToVal a ps
    ToVal (Option a)   ps        = ToValueMaybe (ToVal a ps)
    ToVal a            '[]       = Just (ToConfig a Id)
    ToVal a            (p ': ps) = Nothing

-- | This is '<|>' on 'Maybe' lifted to the type level.
type family OrElse (x :: Maybe k) (y :: Maybe k) :: Maybe k where
    OrElse (Just x) y = Just x
    OrElse Nothing  y = y

-- | Compile-time 'Maybe'.  Type-level 'Just' / 'Nothing' (as produced
-- by 'ToVal') are embedded in each constructor, resp..  Since 'Just'
-- and 'Nothing' are different types, 'CNothing' and 'CJust' can be
-- distinguished by the type checker.
data CMaybe (a :: Maybe *) where
    CNothing :: CMaybe Nothing
    CJust    :: a -> CMaybe (Just a)

-- | This is a version of '<|>' on 'Maybe' for 'CMaybe'.
orElse :: CMaybe a -> CMaybe b -> CMaybe (OrElse a b)
orElse (CJust x) _ = CJust x
orElse CNothing  y = y


-- *** options

-- for selecting optional parts, i don't think i have found the most
-- elegant solution yet.

type family ToValueMaybe (a :: Maybe *) :: Maybe * where
    ToValueMaybe (Just x) = Just (Maybe x)
    ToValueMaybe Nothing  = Nothing

toValueMaybe :: CMaybe a -> CMaybe (ToValueMaybe a)
toValueMaybe (CJust x) = CJust $ Just x
toValueMaybe CNothing  = CNothing

class NothingValue (a :: Maybe *) where
  nothingValue :: Proxy a -> CMaybe (ToValueMaybe a)

instance NothingValue Nothing where
  nothingValue _ = CNothing

instance NothingValue (Just x) where
  nothingValue _ = CJust Nothing


-- *** cfg traversal

-- We need the 'Val' constraint in some instances because overlapping
-- instances and closed type families aren't fully compatible. GHC
-- won't be able to recognize that we've already excluded the other
-- cases and not reduce 'Val' automatically. But the constraints should
-- always resolve, unless we've made a mistake, and the worst outcome
-- if we did are extra type errors, not run-time errors.
class Sel cfg ps where
    sel :: Tagged cfg -> Proxy ps -> CMaybe (ToVal cfg ps)

instance ( cfg ~ Record cfg' cfg''
         -- @ToVal cfg ps ~ Just t@ or @ToVal cfg ps ~ Nothing@
         , Sel cfg' ps
         , Sel cfg'' ps
         ) => Sel (Record cfg' cfg'') ps where
    sel (Tagged (a :- b)) ps = orElse (sel (Tagged a :: Tagged cfg') ps) (sel (Tagged b :: Tagged cfg'') ps)

instance ( cfg ~ Label p cfg'
         -- @ToVal cfg ps ~ Just t@ or @ToVal cfg ps ~ Nothing@
         , t ~ ToConfig cfg Id
         , Sel cfg' ps
         , KnownSymbol p
         ) => Sel (Label p cfg') (p ': ps) where
    sel (Tagged (Id a)) Proxy = sel (Tagged a :: Tagged cfg') (Proxy :: Proxy ps)

instance ( cfg ~ Option cfg'
         , NothingValue (ToVal cfg' ps)
         , Sel cfg' ps
         ) => Sel (Option cfg') ps where
    sel (Tagged NothingO)  _  = nothingValue (Proxy :: Proxy (ToVal cfg' ps))
    sel (Tagged (JustO a)) ps = toValueMaybe $ sel (Tagged a :: Tagged cfg') ps

instance Sel' cfg ps => Sel cfg ps where
    sel = sel'

-- | Helper class for disambiguating overlaps.  The trick is that the
-- 'Sel' instance based on the 'Sel'' constraint is more general
-- than all other instances, so @OverlappingInstances@ will ensure it
-- is matched last.  This way, no instance of 'Sel'' can wrongly
-- overlap with any instance of 'Sel'.
class Sel' cfg ps where
    sel' :: Tagged cfg -> Proxy ps -> CMaybe (ToVal cfg ps)

instance ( t ~ ToConfig cfg Id
         , ToVal cfg '[] ~ Just t
         ) => Sel' cfg '[] where
    sel' (Tagged a) Proxy = CJust a

instance ( ToVal cfg (p ': ps) ~ Nothing
         ) => Sel' cfg (p ': ps) where
    sel' _ _ = CNothing


-- *** static lookup error handling

type ToValE (a :: ConfigCode *) (p :: [Symbol]) = ToExc (LookupFailed a p) (ToVal a p)

data Exc a b = Fail a | Done b

data LookupFailed a p

type family ToExc (a :: k) (x :: Maybe l) :: Exc k l where
  ToExc a Nothing  = Fail a
  ToExc a (Just x) = Done x


-- * merge configs

merge :: forall cfg tm ti .
        ( tm ~ TaggedM cfg
        , ti ~ Tagged cfg
        , Freeze cfg
        , Monoid tm
        , CanonicalizePartial cfg
        ) => [tm] -> Either Error ti
merge = freeze . mconcat . map canonicalizePartial

freeze :: forall cfg tm ti .
        ( tm ~ TaggedM cfg
        , ti ~ Tagged cfg
        , Freeze cfg
        ) => tm -> Either Error ti
freeze = fmap Tagged . frz (Proxy :: Proxy cfg) [] . fromTaggedM

thaw :: forall cfg tm ti .
        ( tm ~ TaggedM cfg
        , ti ~ Tagged cfg
        , Freeze cfg
        ) => ti -> tm
thaw = TaggedM . thw (Proxy :: Proxy cfg) . fromTagged


instance (Monoid (TaggedM a), Monoid (TaggedM b)) => Monoid (TaggedM (Record a b)) where
    mempty =
        TaggedM $ fromTaggedM (mempty :: TaggedM a)
               :- fromTaggedM (mempty :: TaggedM b)
    mappend (TaggedM (x :- y)) (TaggedM (x' :- y')) =
        TaggedM $ (fromTaggedM $ tagA x <> tagA x')
               :- (fromTaggedM $ tagB y <> tagB y')
      where
        tagA v = TaggedM v :: TaggedM a
        tagB v = TaggedM v :: TaggedM b

-- | If one of two configs is 'Nothing', do the expected thing.  If
-- both are 'Just', append the values.
instance (Monoid (TaggedM a)) => Monoid (TaggedM (Label s a)) where
    mempty = TaggedM Nothing
    mappend xt@(TaggedM _)     (TaggedM Nothing)   = xt
    mappend (TaggedM Nothing)  xt'@(TaggedM _)     = xt'
    mappend (TaggedM (Just x)) (TaggedM (Just x')) = TaggedM . Just . fromTaggedM $ tagA x <> tagA x'
      where
        tagA :: ToConfig a Maybe -> TaggedM a
        tagA = TaggedM

-- | There is no @instance Monoid (TaggedM (Type a))@, since there
-- is no reasonable 'mempty'.  Therefore, we offer a specialized
-- instance for labels that map to 'Type'.
instance Monoid (TaggedM (Label s (Type a))) where
    mempty = TaggedM Nothing
    mappend xt@(TaggedM _) (TaggedM Nothing) = xt
    mappend (TaggedM _  )  xt'@(TaggedM _)   = xt'

-- | Lists are initialized empty by default.  Append overwrites left
-- values with right values.  (If we tried to append list elements
-- recursively, there would be awkward questions about matching list
-- lengths.)
instance Monoid (TaggedM (List a)) where
    mempty = TaggedM []
    mappend _ l = l

instance (Monoid (TaggedM a)) => Monoid (TaggedM (Option a)) where
    mempty = TaggedM NothingO
    mappend x                   (TaggedM NothingO)   = x
    mappend (TaggedM NothingO)  x'                   = x'
    mappend (TaggedM (JustO x)) (TaggedM (JustO x')) = TaggedM . JustO . fromTaggedM $ tagA x <> tagA x'
      where
        tagA :: ToConfig a Maybe -> TaggedM a
        tagA = TaggedM


class Freeze c where
    frz :: Proxy c -> [String] -> ToConfig c Maybe -> Either Error (ToConfig c Id)
    thw :: Proxy c -> ToConfig c Id -> ToConfig c Maybe

instance (Freeze a, Freeze b) => Freeze (Record a b) where
    frz Proxy path (x :- y) = do
        x' <- frz (Proxy :: Proxy a) path x
        y' <- frz (Proxy :: Proxy b) path y
        Right $ x' :- y'

    thw Proxy (x :- y) =
        let x' = thw (Proxy :: Proxy a) x in
        let y' = thw (Proxy :: Proxy b) y in
        x' :- y'

instance (KnownSymbol s, Freeze t) => Freeze (Label s t) where
    frz Proxy path = f
      where
        path' = symbolVal (Proxy :: Proxy s) : path
        f (Just x) = Id <$> frz (Proxy :: Proxy t) path' x
        f Nothing  = Left $ FreezeIncomplete path'

    thw Proxy (Id x) = Just $ thw (Proxy :: Proxy t) x

instance (Freeze c) => Freeze (List c) where
    frz Proxy path xs = sequence $ frz (Proxy :: Proxy c) path <$> xs
    thw Proxy xs = thw (Proxy :: Proxy c) <$> xs

-- | FIXME: if a non-optional part of an optional sub-config is
-- missing, the 'FreezeIncomplete' error is ignored and the entire
-- sub-config is cleared.  it would be better to distinguish between
-- the cases `sub-config missing` and `sub-config provided
-- incompletely`, and still raise an error in the latter.
instance ( ToConfig ('Option c) Maybe ~ MaybeO tm
         , ToConfig ('Option c) Id ~ MaybeO ti
         , tm ~ ToConfig c Maybe
         , ti ~ ToConfig c Id
         , Freeze c
         ) => Freeze (Option c) where
    frz :: Proxy ('Option c)
        -> [String]
        -> ToConfig ('Option c) Maybe  -- (if i replace this with @MaybeO tm@, ghc 7.8.4 gives up...  ?)
        -> Either Error (ToConfig ('Option c) Id)
    frz Proxy _    NothingO = Right NothingO
    frz Proxy path (JustO (mx :: tm)) = case frz (Proxy :: Proxy c) path mx of
        Right mx'                 -> Right $ JustO mx'
        Left (FreezeIncomplete _) -> Right NothingO
        Left e                    -> Left e

    thw Proxy (JustO mx) = JustO $ thw (Proxy :: Proxy c) mx
    thw Proxy NothingO   = NothingO

instance Freeze (Type c) where
    frz Proxy _ x = Right x
    thw Proxy x = x


class CanonicalizePartial a where
    canonicalizePartial :: TaggedM a -> TaggedM a
    emptyPartial :: TaggedM a -> Bool

instance (CanonicalizePartial cfg, CanonicalizePartial cfg')
        => CanonicalizePartial (Record cfg cfg') where
    canonicalizePartial (TaggedM (a :- a')) = TaggedM $
            fromTaggedM (canonicalizePartial (TaggedM a :: TaggedM cfg))
         :- fromTaggedM (canonicalizePartial (TaggedM a' :: TaggedM cfg'))

    emptyPartial (TaggedM (a :- a')) =
            emptyPartial (TaggedM a :: TaggedM cfg)
         && emptyPartial (TaggedM a' :: TaggedM cfg')

instance (cfg ~ Label s cfg', CanonicalizePartial cfg')
        => CanonicalizePartial (Label s cfg') where
    canonicalizePartial l@(TaggedM Nothing) = l
    canonicalizePartial l@(TaggedM (Just a)) = if emptyPartial l
        then TaggedM Nothing
        else TaggedM . Just . fromTaggedM . canonicalizePartial . tag $ a
      where
        tag :: ToConfig cfg' Maybe -> TaggedM cfg'
        tag = TaggedM

    emptyPartial (TaggedM Nothing) = True
    emptyPartial (TaggedM (Just a)) = emptyPartial (TaggedM a :: TaggedM cfg')

instance (cfg ~ List cfg', CanonicalizePartial cfg')
        => CanonicalizePartial (List cfg') where
    canonicalizePartial (TaggedM xs) =
          TaggedM . map fromTaggedM . filter (not . emptyPartial) . map (canonicalizePartial . tag) $ xs
      where
        tag :: ToConfig cfg' Maybe -> TaggedM cfg'
        tag = TaggedM

    emptyPartial (TaggedM xs@(_:_)) = all (\ x -> emptyPartial (TaggedM x :: TaggedM cfg')) xs
    emptyPartial (TaggedM []) = True

    -- FIXME: our treatment of lists is confused (overwrite
    -- vs. ignore).  this is particularly appearent here, but applies
    -- to other parts of the code.

instance (cfg ~ Option cfg', CanonicalizePartial cfg')
        => CanonicalizePartial (Option cfg') where
    canonicalizePartial (TaggedM (JustO x)) = if emptyPartial $ tag x
        then TaggedM NothingO
        else TaggedM . JustO . fromTaggedM . canonicalizePartial . tag $ x
      where
        tag :: ToConfig cfg' Maybe -> TaggedM cfg'
        tag = TaggedM

    emptyPartial (TaggedM (JustO x)) = emptyPartial (TaggedM x :: TaggedM cfg')
    emptyPartial (TaggedM NothingO) = True

instance CanonicalizePartial (Type a) where
    canonicalizePartial = id
    emptyPartial _ = False


-- * docs

docs :: ( HasToDoc a
        , HasRenderDoc ConfigFile
        , HasRenderDoc ShellEnv
        , HasRenderDoc CommandLine
        ) => Proxy a -> ST
docs proxy = renderDoc (Proxy :: Proxy ConfigFile)  (toDoc proxy)
          <> renderDoc (Proxy :: Proxy ShellEnv)    (toDoc proxy)
          <> renderDoc (Proxy :: Proxy CommandLine) (toDoc proxy)


data Doc =
    DocDict [(String, Maybe String, Doc, DocOptional)]
  | DocList Doc
  | DocType String
  deriving (Eq, Ord, Show, Read, Typeable)

data DocOptional = DocMandatory | DocOptional
  deriving (Eq, Ord, Show, Read, Typeable)

concatDoc :: Doc -> Doc -> Doc
concatDoc (DocDict xs) (DocDict ys) = DocDict $ xs ++ ys
concatDoc bad bad' = error $ "concatDoc: " ++ show (bad, bad')

class HasToDoc (a :: ConfigCode *) where
    toDoc :: Proxy a -> Doc

instance (HasToDoc a, HasToDoc b) => HasToDoc (Record a b) where
    toDoc Proxy = toDoc (Proxy :: Proxy a) `concatDoc` toDoc (Proxy :: Proxy b)

instance (KnownSymbol path, HasToDoc a) => HasToDoc (Label path a) where
    toDoc Proxy = DocDict
        [( symbolVal (Proxy :: Proxy path)
         , Nothing
         , toDoc (Proxy :: Proxy a)
         , DocMandatory
         )]

instance (HasToDoc a, KnownSymbol path, KnownSymbol descr)
        => HasToDoc (Descr (Label path a) descr) where
    toDoc Proxy = DocDict
        [( symbolVal (Proxy :: Proxy path)
         , Just $ symbolVal (Proxy :: Proxy descr)
         , toDoc (Proxy :: Proxy a)
         , DocMandatory
         )]

instance (HasToDoc a) => HasToDoc (List a) where
    toDoc Proxy = DocList $ toDoc (Proxy :: Proxy a)

instance (HasToDoc a) => HasToDoc (Option a) where
    toDoc Proxy = case toDoc (Proxy :: Proxy a) of
        DocDict xs -> DocDict $ (\ (p, d, t, _) -> (p, d, t, DocOptional)) <$> xs
        bad -> error $ "HasToDoc Option: " ++ show bad

instance (Typeable a) => HasToDoc (Type a) where
    toDoc Proxy = DocType . show $ typeOf (undefined :: a)


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
        f (DocType base) = [base]

        g :: (String, Maybe String, Doc, DocOptional) -> [String]
        g (key, mDescr, subdoc, optional) =
            maybe [] (\ descr -> ["# " <> descr]) mDescr ++
            [ "# [optional]" | case optional of DocMandatory -> False; DocOptional -> True ] ++
            (key <> ":") : indent "  " (f subdoc) ++
            [""]

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
        f (reverse -> acc) (DocType base) =
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

        g :: [(String, Maybe String)] -> (String, Maybe String, Doc, DocOptional) -> [String]
        g acc (key, descr, subdoc, _) = f ((key, descr) : acc) subdoc

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
