{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE EmptyDataDecls        #-}
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
{-# LANGUAGE ViewPatterns          #-}

{-# LANGUAGE UndecidableInstances  #-}  -- is there a way to remove this?

{-# OPTIONS  #-}

module Data.Configifier
where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Exception (Exception, assert)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Identity
import Data.Aeson (ToJSON, FromJSON, Value(Object, Null), object, toJSON, (.=))
import Data.CaseInsensitive (mk)
import Data.Char (toUpper)
import Data.Either.Combinators (mapLeft)
import Data.Function (on)
import Data.List (nubBy, intercalate)
import Data.Maybe (catMaybes)
import Data.String.Conversions (ST, SBS, cs, (<>))
import Data.Typeable (Typeable, Proxy(Proxy), typeOf)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Safe (readMay)
import Text.Show.Pretty (ppShow)

-- (FIXME: can i replace aeson entirely with yaml?  right now, the mix
-- of use of both is rather chaotic.)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Data.Yaml as Yaml
import qualified Text.Regex.Easy as Regex


-- * config types

-- | Construction of config records (@cons@ for record fields).
data a :| b = a :| b
  deriving (Eq, Ord, Show, Typeable)
infixr 6 :|

-- | Construction of config record fields.
data (s :: Symbol) :> (t :: *)
  deriving (Typeable)
infixr 9 :>

-- | Add descriptive text to record field for documentation.
data a :>: (s :: Symbol)
  deriving (Typeable)
infixr 8 :>:


data ConfigCode k =
      Choice (ConfigCode k) (ConfigCode k)
    | Label  Symbol (ConfigCode k)
    | Descr  (ConfigCode k) Symbol
    | List   (ConfigCode k)
    | Option (ConfigCode k)
    | Type   k

infixr 6 `Choice`


-- | Map user-provided config type to 'ConfigCode' types.
type family ToConfigCode (k :: *) :: ConfigCode * where
    ToConfigCode (a :| b)  = Choice (ToConfigCode a) (ToConfigCode b)
    ToConfigCode (s :> a)  = Label s (ToConfigCode a)
    ToConfigCode (a :>: s) = Descr (ToConfigCode a) s
    ToConfigCode [a]       = List (ToConfigCode a)
    ToConfigCode (Maybe a) = Option (ToConfigCode a)
    ToConfigCode a         = Type a

-- | Filter 'Descr' constructors from 'ConfigCode'.
type family NoDesc (k :: ConfigCode *) :: ConfigCode * where
    NoDesc (Choice a b) = Choice (NoDesc a) (NoDesc b)
    NoDesc (Label s a)  = Label s (NoDesc a)
    NoDesc (Descr a s)  = NoDesc a
    NoDesc (List a)     = List (NoDesc a)
    NoDesc (Option a)   = Option (NoDesc a)
    NoDesc (Type a)     = Type a

-- | Map 'ConfgCode' types to the types of config values.
type family ToConfig (k :: ConfigCode *) (f :: * -> *) :: * where
    ToConfig (Choice a b) f = ToConfig a f :| ToConfig b f
    ToConfig (Label s a)  f = f (ToConfig a f)
    ToConfig (List a)     f = [ToConfig a f]
    ToConfig (Option a)   f = Maybe (ToConfig a f)
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


-- * tagged values

data Tagged cfg t = Tagged t
  deriving (Eq, Ord, Show, Typeable)

fromTagged :: Tagged cfg t -> t
fromTagged (Tagged t) = t


-- * results and errors

type Result cfg = Either Error (Tagged cfg (ToConfig cfg Identity))

data Error =  -- FIXME: are these all in use?
      InvalidYaml String
    | ShellEnvNoParse (String, String, String)
    | ShellEnvSegmentNotFound String
    | CommandLinePrimitiveParseError String
    | CommandLinePrimitiveOtherError Error
    | FreezeIncomplete [String]
  deriving (Eq, Ord, Show, Typeable)

instance Exception Error


-- * the main function

configify :: forall cfg mval .
      ( mval ~ Tagged cfg (ToConfig cfg Maybe)
      , Merge cfg
      , FromJSON mval
      , HasParseShellEnv cfg
--      , HasParseCommandLine cfg
      ) => [Source] -> Result cfg
configify [] = error "configify: no sources!"
configify sources = sequence (get <$> sources) >>= merge
  where
    get :: Source -> Either Error mval
    get (ConfigFileYaml sbs) = parseConfigFile sbs
    get (ShellEnv env)       = parseShellEnv env
--    get (CommandLine args)   = parseCommandLine proxy args



-- * yaml / json

parseConfigFile :: ( t ~ Tagged cfg (ToConfig cfg Maybe)
                   , FromJSON t
                   )
        => SBS -> Either Error t
parseConfigFile = mapLeft InvalidYaml . Yaml.decodeEither

renderConfigFile :: ( t ~ Tagged cfg (ToConfig cfg Identity)
                    , t' ~ Tagged cfg (ToConfig cfg Maybe)
                    , ToJSON t'
                    , Merge cfg
                    )
        => t -> SBS
renderConfigFile = Yaml.encode . thaw


-- ** render json

-- | @instance ToJSON Choice@
instance ( t1 ~ ToConfig cfg1 Maybe, ToJSON (Tagged cfg1 t1)
         , t2 ~ ToConfig cfg2 Maybe, ToJSON (Tagged cfg2 t2)
         , t' ~ ToConfig (Choice cfg1 cfg2) Maybe
         )
        => ToJSON (Tagged (Choice cfg1 cfg2) t') where
    toJSON (Tagged (o1 :| o2)) = case ( toJSON (Tagged o1 :: Tagged cfg1 t1)
                                      , toJSON (Tagged o2 :: Tagged cfg2 t2)
                                      ) of
        (Object m1, Object m2) -> Object $ HashMap.union m2 m1
        (_, v')                -> v'

-- | @instance ToJSON Label@
instance ( t ~ ToConfig cfg Maybe, ToJSON (Tagged cfg t)
         , t' ~ ToConfig (Label s cfg) Maybe, KnownSymbol s
         )
        => ToJSON (Tagged (Label s cfg) t') where
    toJSON (Tagged Nothing) = object []
    toJSON (Tagged (Just v)) = if val == Aeson.Null then object [] else object [key .= val]
      where
        key = cs $ symbolVal (Proxy :: Proxy s)
        val = toJSON (Tagged v :: Tagged cfg t)

-- | @instance ToJSON List@
instance (t ~ ToConfig cfg Maybe, ToJSON (Tagged cfg t))
        => ToJSON (Tagged (List cfg) [t]) where
    toJSON (Tagged vs) = toJSON $ (Tagged :: t -> Tagged cfg t) <$> vs

-- | @instance ToJSON Option@
instance (t ~ ToConfig cfg Maybe, ToJSON (Tagged cfg t))
        => ToJSON (Tagged (Option cfg) (Maybe t)) where
    toJSON (Tagged mv) = toJSON $ (Tagged :: t -> Tagged cfg t) <$> mv

-- | @instance ToJSON Type@
instance (ToJSON a) => ToJSON (Tagged (Type a) a) where
    toJSON (Tagged v) = toJSON v


-- ** parse json

-- | @instance FromJSON Choice@
instance ( t1 ~ ToConfig cfg1 Maybe, FromJSON (Tagged cfg1 t1)
         , t2 ~ ToConfig cfg2 Maybe, FromJSON (Tagged cfg2 t2)
         , t' ~ ToConfig (Choice cfg1 cfg2) Maybe
         )
        => FromJSON (Tagged (Choice cfg1 cfg2) t') where
    parseJSON json = do
        Tagged o1 :: Tagged cfg1 t1 <- Aeson.parseJSON json
        Tagged o2 :: Tagged cfg2 t2 <- Aeson.parseJSON json
        return . Tagged $ o1 :| o2

-- | @instance FromJSON Label@ (tolerates unknown fields in json object.)
instance ( t ~ ToConfig cfg Maybe, FromJSON (Tagged cfg t)
         , t' ~ ToConfig (Label s cfg) Maybe, KnownSymbol s
         )
        => FromJSON (Tagged (Label s cfg) t') where
    parseJSON = Aeson.withObject "configifier object" $ \ m ->
          case HashMap.lookup key m of
            (Just json) -> Tagged . Just . fromTagged <$> parseJSON' json
            Nothing     -> return $ Tagged Nothing
        where
          key = cs $ symbolVal (Proxy :: Proxy s)
          parseJSON' :: Aeson.Value -> Aeson.Parser (Tagged cfg t) = Aeson.parseJSON

-- | @instance ParseJSON List@
instance ( t ~ ToConfig cfg Maybe, FromJSON (Tagged cfg t)
         , t' ~ ToConfig (List cfg) Maybe
         )
        => FromJSON (Tagged (List cfg) t') where
    parseJSON = Aeson.withArray "configifier list" $ \ vector -> do
        vector' :: [Tagged cfg t] <- sequence $ Aeson.parseJSON <$> Vector.toList vector
        return . Tagged . (fromTagged <$>) $ vector'

-- | @instance ParseJSON Option@
instance ( t ~ ToConfig cfg Maybe, FromJSON (Tagged cfg t)
         , t' ~ ToConfig (Option cfg) Maybe
         )
        => FromJSON (Tagged (Option cfg) t') where
    parseJSON Null = return (Tagged Nothing :: Tagged (Option cfg) t')
    parseJSON v = do
        Tagged js :: Tagged cfg t <- Aeson.parseJSON v
        return $ (Tagged (Just js) :: Tagged (Option cfg) t')

-- | @instance FromJSON Type@
instance (FromJSON a) => FromJSON (Tagged (Type a) a) where
    parseJSON = (Tagged <$>) . Aeson.parseJSON


-- * shell env.

type Env = [(String, String)]

class HasParseShellEnv (cfg :: ConfigCode *) where
    parseShellEnv :: Env -> Either Error (Tagged cfg (ToConfig cfg Maybe))

instance (HasParseShellEnv a, HasParseShellEnv b) => HasParseShellEnv (Choice a b) where
    parseShellEnv env = do
        Tagged x :: Tagged a (ToConfig a Maybe) <- parseShellEnv env
        Tagged y :: Tagged b (ToConfig b Maybe) <- parseShellEnv env
        return . Tagged $ x :| y

-- | The paths into the recursive structure of the config file are
-- concatenated to shell variable names with separating '_'.  (It is
-- still ok to have '_' in your config path names.  This parser chops
-- off complete matching names, whether they contain '_' or not, and
-- only then worries about trailing '_'.)
instance (KnownSymbol path, HasParseShellEnv a) => HasParseShellEnv (Label path a) where
    parseShellEnv env = if null env'
          then Left $ ShellEnvSegmentNotFound key
          else do
              Tagged a :: Tagged a (ToConfig a Maybe) <- parseShellEnv env'
              return $ Tagged (Just a)
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
        Tagged a :: Tagged a (ToConfig a Maybe) <- parseShellEnv env
        return $ Tagged [a]

instance HasParseShellEnv a => HasParseShellEnv (Option a) where
    parseShellEnv env = do
        Tagged a :: Tagged a (ToConfig a Maybe) <- parseShellEnv env
        return $ Tagged (Just a)

instance (Typeable a, FromJSON (Tagged (Type a) a)) => HasParseShellEnv (Type a) where
    parseShellEnv [("", s)] = mapLeft renderError (Yaml.decodeEither (cs s) :: Either String (Tagged (Type a) a))
      where
        renderError :: String -> Error
        renderError e = ShellEnvNoParse (show $ typeOf (undefined :: a), s, e)


-- * cli

{-

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

-}


-- * accessing config values

-- ** exposed interface

{-
-- | This is a wrapper around 'sel' that hides the interal use of
-- 'CMaybe'.  As we expect, this version will just cause a type error
-- if it is applied to an illegal path.
(>.) :: forall cfg a p r . (Sel a p, ValE a p ~ Done r) => a -> Proxy p -> r
(>.) cfg p = case sel cfg p of
    CJust x  -> x
    _        -> error "inaccessible"
-}


-- * more type-level programming...

-- | Type-level lookup of a path in a configuration type.
-- A path is represented as a list of symbols.
type family Val (k :: ConfigCode *) (p :: [Symbol]) :: Maybe * where
    Val (Choice a b) (p ': ps) = OrElse (Val a (p ': ps)) (Val b (p ': ps))
    Val (Label p a)  (p ': ps) = Identity (Val a ps)
    Val (List a)     (p ': ps) = Val a (p ': ps)
    Val (Option a)   (p ': ps) = Val a (p ': ps)
    Val (Type a)     '[]       = Just a
    Val a            ps        = Nothing

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


class Sel cfg ps v where
    sel :: Proxy cfg -> Proxy ps -> v -> CMaybe (Val cfg ps)

{-
instance ( Val ('Choice cfg cfg') ps ~ OrElse (Val cfg ps) (Val cfg' ps)
         ) => Sel (Choice cfg cfg') (v1 :| v2) ps where
    sel Proxy ps (x :| y) = combine _ _ :: CMaybe (Val ('Choice cfg cfg') ps)
      where
        x' = sel (Proxy :: Proxy cfg) ps x
        y' = sel (Proxy :: Proxy cfg') ps y
-}


instance ( v ~ ToConfig (Label p cfg) Identity
         , Sel cfg ps (Val cfg ps)
         ) => Sel (Label p cfg) (p ': ps) v where
    sel Proxy Proxy v = case sel (Proxy :: Proxy cfg) (Proxy :: Proxy ps) v of
        x -> _

--     Could not deduce (Sel cfg ps (Identity (ToConfig cfg Identity)))
--       arising from a use of ‘sel’


instance Sel (List a) ps v where
    sel = _

instance Sel (Option a) ps v where
    sel = _



instance Sel (Type a) '[] a where
    sel Proxy Proxy x = CJust x


{-
-- | We need the 'Val' constraint here because overlapping instances
-- and closed type families aren't fully compatible.  GHC won't be
-- able to recognize that we've already excluded the other cases and
-- not reduce 'Val' automatically.  But the constraint should always
-- resolve, unless we've made a mistake, and the worst outcome if we
-- did are extra type errors, not run-time errors.
instance (Val cfg ps ~ Nothing) => Sel cfg ps v where
    sel _ _ _ = CNothing
-}



{-

-- ** better errors

data Exc a b = Fail a | Done b

data LookupFailed a p

type ValE (a :: *) (p :: [Symbol]) = ToExc (LookupFailed a p) (Val a p)

type family ToExc (a :: k) (x :: Maybe l) :: Exc k l where
    ToExc a Nothing  = Fail a
    ToExc a (Just x) = Done x

-}


-- * merge configs

merge :: forall cfg . Merge cfg
        => [Tagged cfg (ToConfig cfg Maybe)]
        -> Either Error (Tagged cfg (ToConfig cfg Identity))
merge = freeze . f
  where
    f (fmap fromTagged -> x:xs) = Tagged $ foldl (mrg (Proxy :: Proxy cfg)) x xs
    f [] = error "merge: empty list."

freeze :: forall cfg . Merge cfg
        => Tagged cfg (ToConfig cfg Maybe)
        -> Either Error (Tagged cfg (ToConfig cfg Identity))
freeze = fmap Tagged . frz (Proxy :: Proxy cfg) [] . fromTagged

thaw :: forall cfg . Merge cfg
        => Tagged cfg (ToConfig cfg Identity)
        -> Tagged cfg (ToConfig cfg Maybe)
thaw = Tagged . thw (Proxy :: Proxy cfg) . fromTagged


class Merge c where
    mrg :: Proxy c -> ToConfig c Maybe -> ToConfig c Maybe -> ToConfig c Maybe
    frz :: Proxy c -> [String] -> ToConfig c Maybe -> Either Error (ToConfig c Identity)
    thw :: Proxy c -> ToConfig c Identity -> ToConfig c Maybe

instance (Merge a, Merge b) => Merge (Choice a b) where
    mrg Proxy (x :| y) (x' :| y') =
           mrg (Proxy :: Proxy a) x x'
        :| mrg (Proxy :: Proxy b) y y'

    frz Proxy path (x :| y) = do
        x' <- frz (Proxy :: Proxy a) path x
        y' <- frz (Proxy :: Proxy b) path y
        Right $ x' :| y'

    thw Proxy (x :| y) =
        let x' = thw (Proxy :: Proxy a) x in
        let y' = thw (Proxy :: Proxy b) y in
        x' :| y'

instance (KnownSymbol s, Merge t) => Merge (Label s t) where
    mrg Proxy (Just x) (Just y) = Just $ mrg (Proxy :: Proxy t) x y
    mrg Proxy mx my = my <|> mx

    frz Proxy path = f
      where
        path' = symbolVal (Proxy :: Proxy s) : path
        f (Just x) = Identity <$> frz (Proxy :: Proxy t) path' x
        f Nothing  = Left $ FreezeIncomplete path'

    thw Proxy (Identity x) = Just $ thw (Proxy :: Proxy t) x

instance (Merge c) => Merge (List c) where
    mrg Proxy _ ys = ys
    frz Proxy path xs = sequence $ frz (Proxy :: Proxy c) path <$> xs
    thw Proxy xs = thw (Proxy :: Proxy c) <$> xs

-- | FIXME: if an optional sub-config is provided incompletely, the
-- 'FreezeIncomplete' error will be dropped and the entire sub-config
-- is cleared.  we may want to distinguish between the cases
-- `sub-config missing` and `sub-config provided incompletely`
-- instead.
instance ( ToConfig ('Option c) Maybe ~ Maybe tm
         , ToConfig ('Option c) Identity ~ Maybe ti
         , tm ~ ToConfig c Maybe
         , ti ~ ToConfig c Identity
         , Merge c
         ) => Merge (Option c) where
    mrg Proxy mx my = my <|> mx

    frz :: Proxy ('Option c)
        -> [String]
        -> ToConfig ('Option c) Maybe  -- (if i replace this with @Maybe tm@, ghc 7.8.4 gives up.)
        -> Either Error (ToConfig ('Option c) Identity)
    frz Proxy path (Just (mx :: tm)) = Just <$> frz (Proxy :: Proxy c) path mx
    frz Proxy path Nothing           = Right Nothing


{-

FIXME: 'Option' does not work yet if something is legitimately
missing.

-- traceShow ("***", path, mx, frz (Proxy :: Proxy c) path mx) $

("***",["frontend"],Just "localhost",Right (Identity "localhost"))
("***",["backend"],Nothing,Left (FreezeIncomplete ["expose_host","backend"]))
("***",[],Just (Just 8002 :| (Just "localhost" :| Just Nothing)),Left (FreezeIncomplete ["expose_host","backend"]))
("***",["backend"],Nothing,Left (FreezeIncomplete ["expose_host","backend"]))

- why does ["backend"] occur twice?
- more importantly, why doesn't it freeze successfully?

-}


    thw Proxy (Just mx) = Just $ thw (Proxy :: Proxy c) mx
    thw Proxy Nothing   = Nothing

instance Merge (Type c) where
    mrg Proxy _ y = y
    frz Proxy _ x = Right x
    thw Proxy x = x


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

instance (HasToDoc a, HasToDoc b) => HasToDoc (Choice a b) where
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
