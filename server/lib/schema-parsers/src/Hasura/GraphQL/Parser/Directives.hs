-- | Definition of all supported GraphQL directives.
module Hasura.GraphQL.Parser.Directives
  ( -- list of directives, for the schema
    directivesInfo,
    inclusionDirectives,
    customDirectives,
    -- Custom Directive Types
    CachedDirective (..),
    DirectiveMap,
    -- lookup keys for directives
    include,
    skip,
    cached,
    multipleRootFields,
    -- parsing utilities
    parseDirectives,
    withDirective,
    -- exposed for tests
    Directive (..),
    DirectiveKey (..),
    skipDirective,
    includeDirective,
    cachedDirective,
    multipleRootFieldsDirective,
  )
where

import Control.Monad (guard, unless)
import Data.Aeson.Key qualified as K
import Data.Aeson.Types (JSONPathElement (Key))
import Data.Dependent.Map qualified as DM
import Data.Dependent.Sum (DSum (..))
import Data.Foldable (for_)
import Data.Functor.Identity (Identity (..))
import Data.GADT.Compare.Extended
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as S
import Data.List qualified as L
import Data.Traversable (for)
import Data.Typeable (eqT)
import Hasura.Base.ToErrorValue
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Parser.DirectiveName qualified as Name
import Hasura.GraphQL.Parser.Internal.Input
import Hasura.GraphQL.Parser.Internal.Scalars
import Hasura.GraphQL.Parser.Schema
import Hasura.GraphQL.Parser.Variable
import Language.GraphQL.Draft.Syntax qualified as G
import Type.Reflection (Typeable, typeRep)
import Witherable (catMaybes)
import Prelude

-- Disable custom prelude warnings in preparation for extracting this module into a separate package.
{-# ANN module ("HLint: ignore Use onNothing" :: String) #-}

-- | Returns the schema information for all supported directives, for each of
-- which a @DirectiveDefinition@ will be inserted into the schema.
--
-- To add a new custom directives to the schema, add it to @customDirectives@.
-- Inclusion directives are treated separately, as they are not concerned with
-- altering the result of the parsers, but about whether a given field should be
-- parsed at all (see @collectFields@ for more information).
--
-- Alternatively, you could manually add a @DirectiveInfo@ to this list;
-- however, if you do so, and do not provide a corresponding definition in
-- @customDirectives@, calls to @parseDirectives@ will NOT attempt to parse that
-- new directive: they will only validate that it is used correctly. You can use
-- this option to implement directives that do not match our simple inclusion /
-- custom dichotomy.
--
-- To create a directive definition, you need to create a @Directive@ using
-- @mkDirective@, and export from this module its corresponding @DirectiveKey@
-- (see @skip@ below for an example). Make sure the type in the key matches the
-- type of the parser!
--
-- Directives may be "hidden", in which case they won't advertised in the
-- schema, but silently accepted. This is un-advisable and should only be used
-- when there's no other way around it.
directivesInfo :: forall m origin. (MonadParse m) => [DirectiveInfo origin]
directivesInfo = do
  dir <- inclusionDirectives @m <> customDirectives @m
  guard $ dAdvertised dir
  pure $ dDefinition dir

-- | Not exported, only used internally; identical to 'directivesInfo', but also
-- contains hidden directives.
allDirectives :: forall m origin. (MonadParse m) => [DirectiveInfo origin]
allDirectives = map dDefinition $ inclusionDirectives @m <> customDirectives @m

inclusionDirectives :: forall m origin. (MonadParse m) => [Directive origin m]
inclusionDirectives = [includeDirective @m, skipDirective @m]

customDirectives :: forall m origin. (MonadParse m) => [Directive origin m]
customDirectives = [cachedDirective @m, multipleRootFieldsDirective @m]

-- | Parses directives, given a location. Ensures that all directives are known
-- and match the location; subsequently builds a dependent map of the results,
-- that can be then introspected with @withDirective@. The list of parsers that
-- should be applied is given as an argument: if a valid directive is found, but
-- for which no parser is provided, it will be ignored.
--
-- Example use:
--
--     dMap <- parseDirectives customDirectives (DLExecutable EDLQUERY) directives
--     withDirective dMap cached $ for_ \_ -> tagAsCached
parseDirectives ::
  forall origin m.
  (MonadParse m) =>
  [Directive origin m] ->
  G.DirectiveLocation ->
  [G.Directive Variable] ->
  m DirectiveMap
{-# INLINE parseDirectives #-}
parseDirectives directiveParsers location givenDirectives = do
  result <-
    catMaybes <$> for givenDirectives \directive -> do
      let name = G._dName directive
      -- check the directive has a matching definition
      DirectiveInfo {diLocations} <-
        maybe (parseError ("directive " <> toErrorValue name <> " is not defined in the schema")) pure $
          L.find (\di -> diName di == name) (allDirectives @m)
      -- check that it is allowed at the current location
      unless (location `elem` diLocations) $
        parseError $
          "directive " <> toErrorValue name <> " is not allowed on " <> humanReadable location
      -- if we are expecting to parse it now, create a dmap entry
      case L.find (\d -> diName (dDefinition d) == name) directiveParsers of
        Nothing -> pure Nothing
        Just (Directive {dParser}) -> do
          result <- dParser directive
          pure $ Just (name, DirectiveKey name :=> pure result)
  -- check that the result does not contain duplicates
  let dups = duplicates $ fst <$> result
  unless (null dups) $
    parseError $
      "the following directives are used more than once: " <> toErrorValue dups
  pure $ DM.fromList $ snd <$> result
  where
    humanReadable = \case
      G.DLExecutable G.EDLQUERY -> "a query"
      G.DLExecutable G.EDLMUTATION -> "a mutation"
      G.DLExecutable G.EDLSUBSCRIPTION -> "a subscription"
      G.DLExecutable G.EDLFIELD -> "a field"
      G.DLExecutable G.EDLFRAGMENT_DEFINITION -> "a fragment definition"
      G.DLExecutable G.EDLFRAGMENT_SPREAD -> "a fragment spread"
      G.DLExecutable G.EDLINLINE_FRAGMENT -> "an inline fragment"
      G.DLTypeSystem G.TSDLSCHEMA -> "the schema"
      G.DLTypeSystem G.TSDLSCALAR -> "a scalar definition"
      G.DLTypeSystem G.TSDLOBJECT -> "an object definition"
      G.DLTypeSystem G.TSDLFIELD_DEFINITION -> "a field definition"
      G.DLTypeSystem G.TSDLARGUMENT_DEFINITION -> "an argument definition"
      G.DLTypeSystem G.TSDLINTERFACE -> "an interface definition"
      G.DLTypeSystem G.TSDLUNION -> "an union definition"
      G.DLTypeSystem G.TSDLENUM -> "an enum definition"
      G.DLTypeSystem G.TSDLENUM_VALUE -> "an enum value definition"
      G.DLTypeSystem G.TSDLINPUT_OBJECT -> "an input object definition"
      G.DLTypeSystem G.TSDLINPUT_FIELD_DEFINITION -> "an input field definition"
    duplicates = S.fromList . HashMap.keys . HashMap.filter (> 1) . HashMap.fromListWith (+) . map (,1 :: Int)

withDirective ::
  DirectiveMap ->
  DirectiveKey a ->
  (Maybe a -> m b) ->
  m b
withDirective dmap key callback = callback $ runIdentity <$> DM.lookup key dmap

-- Cached custom directive.

cachedDirective :: forall m origin. (MonadParse m) => Directive origin m
cachedDirective =
  mkDirective
    Name._cached
    (Just "whether this query should be cached (Hasura Cloud only)")
    True
    [G.DLExecutable G.EDLQUERY]
    (CachedDirective <$> ttlArgument <*> forcedArgument)
  where
    -- Optionally set the cache entry time to live
    ttlArgument :: InputFieldsParser origin m Int
    ttlArgument = fieldWithDefault Name._ttl (Just "measured in seconds") (G.VInt 60) $ fromIntegral <$> int

    -- Optionally Force a refresh of the cache entry
    forcedArgument :: InputFieldsParser origin m Bool
    forcedArgument = fieldWithDefault Name._refresh (Just "refresh the cache entry") (G.VBoolean False) boolean

data CachedDirective = CachedDirective {cdTtl :: Int, cdRefresh :: Bool}

cached :: DirectiveKey CachedDirective
cached = DirectiveKey Name._cached

-- Subscription tests custom directive.

multipleRootFieldsDirective :: (MonadParse m) => Directive origin m
multipleRootFieldsDirective =
  mkDirective
    Name.__multiple_top_level_fields
    (Just "INTERNAL TESTING TOOL DO NOT USE")
    False -- not advertised in the schema
    [G.DLExecutable G.EDLSUBSCRIPTION]
    (pure ())

multipleRootFields :: DirectiveKey ()
multipleRootFields = DirectiveKey Name.__multiple_top_level_fields

-- Built-in inclusion directives

skipDirective :: (MonadParse m) => Directive origin m
skipDirective =
  mkDirective
    Name._skip
    (Just "whether this query should be skipped")
    True
    [ G.DLExecutable G.EDLFIELD,
      G.DLExecutable G.EDLFRAGMENT_SPREAD,
      G.DLExecutable G.EDLINLINE_FRAGMENT
    ]
    ifArgument

includeDirective :: (MonadParse m) => Directive origin m
includeDirective =
  mkDirective
    Name._include
    (Just "whether this query should be included")
    True
    [ G.DLExecutable G.EDLFIELD,
      G.DLExecutable G.EDLFRAGMENT_SPREAD,
      G.DLExecutable G.EDLINLINE_FRAGMENT
    ]
    ifArgument

skip :: DirectiveKey Bool
skip = DirectiveKey Name._skip

include :: DirectiveKey Bool
include = DirectiveKey Name._include

ifArgument :: (MonadParse m) => InputFieldsParser origin m Bool
ifArgument = field Name._if Nothing boolean

-- Parser type for directives.

data Directive origin m where
  Directive ::
    forall origin m a.
    (MonadParse m, Typeable a) =>
    { dDefinition :: DirectiveInfo origin,
      dAdvertised :: Bool,
      dParser :: G.Directive Variable -> m a
    } ->
    Directive origin m

data DirectiveKey a where
  DirectiveKey :: (Typeable a) => G.Name -> DirectiveKey a

instance GEq DirectiveKey where
  geq
    (DirectiveKey name1 :: DirectiveKey a1)
    (DirectiveKey name2 :: DirectiveKey a2)
      | name1 == name2,
        Just Refl <- eqT @a1 @a2 =
          Just Refl
      | otherwise = Nothing

instance GCompare DirectiveKey where
  gcompare
    (DirectiveKey name1 :: DirectiveKey a1)
    (DirectiveKey name2 :: DirectiveKey a2) =
      strengthenOrdering (compare name1 name2)
        `extendGOrdering` gcompare (typeRep @a1) (typeRep @a2)
        `extendGOrdering` GEQ

type DirectiveMap = DM.DMap DirectiveKey Identity

mkDirective ::
  (MonadParse m, Typeable a) =>
  G.Name ->
  Maybe G.Description ->
  Bool ->
  [G.DirectiveLocation] ->
  InputFieldsParser origin m a ->
  Directive origin m
{-# INLINE mkDirective #-}
mkDirective name description advertised location argsParser =
  Directive
    { dDefinition = DirectiveInfo name description (ifDefinitions argsParser) location,
      dAdvertised = advertised,
      dParser = \(G.Directive _name arguments) -> withKey (Key $ K.fromText $ G.unName name) $ do
        for_ (HashMap.keys arguments) \argumentName ->
          unless (argumentName `S.member` argumentNames) $
            parseError $
              toErrorValue name <> " has no argument named " <> toErrorValue argumentName
        withKey (Key $ K.fromText "args") $ ifParser argsParser $ GraphQLValue <$> arguments
    }
  where
    argumentNames = S.fromList (dName <$> ifDefinitions argsParser)
