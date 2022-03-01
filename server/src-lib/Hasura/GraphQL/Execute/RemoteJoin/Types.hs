{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.GraphQL.Execute.RemoteJoin.Types
  ( RemoteJoin (..),
    getPhantomFields,
    getJoinColumnMapping,
    getRemoteSchemaJoins,
    RemoteSchemaJoin (..),
    RemoteSourceJoin (..),
    RemoteJoins,
    JoinColumnAlias (..),
    getAliasFieldName,
    JoinTree (..),
    JoinNode (..),
    JoinCallId,
    JoinArgumentId,
    JoinArgument (..),
    JoinIndex,
    JoinArguments (..),
  )
where

import Control.Lens (view, _1)
import Data.Aeson.Ordered qualified as AO
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.NonEmpty qualified as Map
import Data.IntMap.Strict qualified as IntMap
import Hasura.GraphQL.Parser qualified as P
import Hasura.Prelude
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.Types
import Hasura.SQL.AnyBackend qualified as AB
import Language.GraphQL.Draft.Syntax qualified as G

-- | A JoinTree represents the set of operations that need to be executed to
-- enrich the response of a source with data from remote sources. A tree
-- structure is used to capture the locations in the response where the join
-- has to happpen as it offers an efficient traversal mechanism.
--
-- For a query such as this:
--
-- {
--   city {
--     name
--     code
--     # weather is a remote relationship
--     weather { forecast }
--     state {
--     # weather is a remote relationship
--       weather { forecast }
--     }
--   }
--  }
--
--  the join tree would look like
--  [
--  , ("weather", Leaf RemoteJoinInfoOfWeather),
--  , ("state", [ ("weather", Leaf RemoteJoinInfoOfWeather) ])
--  ]
--
-- Note that the same join tree will be emitted even if 'city' is of type
-- '[City]' and 'state' is of type [State], we currently do not capture any
-- information if any of the fields in the path expect json arrays. It is
-- similar in spirit to a GraphQL selection set in this regard
newtype JoinTree a = JoinTree {unJoinTree :: Map.NEHashMap FieldName (JoinNode a)}
  deriving stock (Eq, Foldable, Functor, Generic, Traversable, Show)
  deriving newtype (Semigroup)

data JoinNode a
  = Leaf a
  | Tree !(JoinTree a)
  deriving stock (Eq, Foldable, Functor, Generic, Traversable, Show)

type RemoteJoins = JoinTree RemoteJoin

-- | TODO(jkachmar): Documentation
data RemoteJoin
  = RemoteJoinSource !(AB.AnyBackend RemoteSourceJoin) !(Maybe RemoteJoins)
  | RemoteJoinRemoteSchema !RemoteSchemaJoin
  deriving stock (Eq, Generic)

-- | This collects all the remote joins from a join tree
getRemoteSchemaJoins :: RemoteJoins -> [RemoteSchemaJoin]
getRemoteSchemaJoins = mapMaybe getRemoteSchemaJoin . toList
  where
    getRemoteSchemaJoin :: RemoteJoin -> Maybe RemoteSchemaJoin
    getRemoteSchemaJoin = \case
      RemoteJoinSource _ _ -> Nothing
      RemoteJoinRemoteSchema s -> Just s

getPhantomFields :: RemoteJoin -> [FieldName]
getPhantomFields =
  mapMaybe getPhantomFieldName . Map.elems . getJoinColumnMapping
  where
    getPhantomFieldName :: JoinColumnAlias -> Maybe FieldName
    getPhantomFieldName = \case
      JCSelected _ -> Nothing
      JCPhantom f -> Just f

getJoinColumnMapping :: RemoteJoin -> Map.HashMap FieldName JoinColumnAlias
getJoinColumnMapping = \case
  RemoteJoinSource sourceJoin _ -> AB.runBackend
    sourceJoin
    \RemoteSourceJoin {_rsjJoinColumns} ->
      fmap (view _1) _rsjJoinColumns
  RemoteJoinRemoteSchema RemoteSchemaJoin {_rsjJoinColumnAliases} ->
    _rsjJoinColumnAliases

-- | TODO(jkachmar): Documentation.
data RemoteSourceJoin b = RemoteSourceJoin
  { _rsjSource :: !SourceName,
    _rsjSourceConfig :: !(SourceConfig b),
    _rsjRelationship :: !(IR.SourceRelationshipSelection b Void P.UnpreparedValue),
    _rsjJoinColumns :: !(Map.HashMap FieldName (JoinColumnAlias, ScalarType b, Column b))
  }
  deriving (Generic)

deriving instance
  ( Backend b,
    Show (ScalarValue b),
    Show (SourceConfig b),
    Show (BooleanOperators b (P.UnpreparedValue b))
  ) =>
  Show (RemoteSourceJoin b)

deriving instance
  ( Backend b,
    Eq (ScalarValue b),
    Eq (BooleanOperators b (P.UnpreparedValue b))
  ) =>
  Eq (RemoteSourceJoin b)

-- | Disambiguates between 'FieldName's which are provided as part of the
-- GraphQL selection provided by the user (i.e. 'JCSelected') and those which
-- we need to retreive data but which are not expressly requested (i.e.
-- 'JCPhantom').
--
-- After processing the remote join, we remove all phantom 'FieldName's and
-- only return those which fall under the 'JCSelected' branch of this type.
data JoinColumnAlias
  = -- | This fieldname is already part of the response.
    JCSelected !FieldName
  | -- | This is explicitly added for the join.
    --
    -- Such keys will have to be removed from the response eventually.
    JCPhantom !FieldName
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable)

getAliasFieldName :: JoinColumnAlias -> FieldName
getAliasFieldName = \case
  JCSelected f -> f
  JCPhantom f -> f

-- | A 'RemoteSchemaJoin' represents the context of a remote relationship to be
-- extracted from 'AnnFieldG's.
data RemoteSchemaJoin = RemoteSchemaJoin
  { -- | User-provided arguments with variables.
    _rsjArgs :: !(Map.HashMap G.Name (P.InputValue RemoteSchemaVariable)),
    -- | Customizer for JSON result from the remote server.
    _rsjResultCustomizer :: !ResultCustomizer,
    -- | User-provided selection set of remote field.
    _rsjSelSet :: !(G.SelectionSet G.NoFragments RemoteSchemaVariable),
    -- | A map of the join column to its alias in the response
    _rsjJoinColumnAliases :: !(Map.HashMap FieldName JoinColumnAlias),
    -- | Remote server fields.
    _rsjFieldCall :: !(NonEmpty FieldCall),
    -- | The remote schema server info.
    _rsjRemoteSchema :: !RemoteSchemaInfo
  }
  deriving stock (Generic)

-- NOTE: This cannot be derived automatically, as 'RemoteResultCustomizer' does
-- not permit a proper 'Eq' instance (it's a newtype around a function).
instance Eq RemoteSchemaJoin where
  (==) = on (==) \RemoteSchemaJoin {..} ->
    (_rsjArgs, _rsjSelSet, _rsjJoinColumnAliases, _rsjFieldCall, _rsjRemoteSchema)

-- | A unique id that gets assigned to each 'RemoteJoin' (this is to avoid the
-- requirement of Ord/Hashable implementation for RemoteJoin)
type JoinCallId = Int

-- | A map of fieldname to values extracted from each LHS row/object
--
-- For example, if a remote relationship 'weather' on 'city' table
-- is defined as follows:
--   city.weather = get_weather(city: city.code, cityState: city.state_code)
-- a join argument for this join would have the values of columns 'code' and
-- 'state_code' for each 'city' row that participates in the join
newtype JoinArgument = JoinArgument {unJoinArgument :: Map.HashMap FieldName AO.Value}
  deriving stock (Eq, Generic, Show)
  deriving newtype (Hashable)

-- | A unique id assigned to each join argument
type JoinArgumentId = Int

-- | A map of JoinArgumentId to its value fetched from the RHS source of a join
type JoinIndex = IntMap.IntMap AO.Value

data JoinArguments = JoinArguments
  { -- | The 'RemoteJoin' associated with the join arguments within this
    -- structure.
    _jalJoin :: !RemoteJoin,
    -- | Arguments for which we must fetch a response from the remote, along with
    -- the identifiers that are used to stitch the final response together.
    --
    -- NOTE: 'Map.HashMap' is used to deduplicate multiple 'JoinArgument's so that
    -- we avoid fetching more data from a remote than is necessary (i.e. in the
    -- case of duplicate arguments).
    _jalArguments :: !(Map.HashMap JoinArgument JoinArgumentId),
    -- | The 'FieldName' associated with the "replacement token" for this join
    -- argument.
    --
    -- NOTE: We need this for query logging; ideally we would use the full path
    -- for the GraphQL query associated with this remote join, but we don't have
    -- access to that here so this is the next best thing to do.
    _jalFieldName :: !FieldName
  }
  deriving stock (Generic)
