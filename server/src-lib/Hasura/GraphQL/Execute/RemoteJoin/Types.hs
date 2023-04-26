{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.GraphQL.Execute.RemoteJoin.Types
  ( -- * Remote joins tree
    JoinTree (..),
    JoinNode (..),
    RemoteJoins,
    QualifiedFieldName (..),
    getRemoteSchemaJoins,

    -- * Individual join information
    RemoteJoin (..),
    JoinCallId,
    JoinColumnAlias (..),
    getAliasFieldName,
    getPhantomFields,
    getJoinColumnMapping,

    -- * Join to source
    RemoteSourceJoin (..),

    -- * Join to schema
    RemoteSchemaJoin (..),

    -- * Join arguments
    JoinArgumentId,
    JoinArgument (..),
    JoinArguments (..),
  )
where

import Data.Aeson.Ordered qualified as AO
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.NonEmpty qualified as NEMap
import Hasura.GraphQL.Parser qualified as P
import Hasura.Prelude
import Hasura.RQL.IR.RemoteSchema qualified as IR
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.IR.Value qualified as IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ResultCustomization
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.RemoteSchema.SchemaCache
import Hasura.SQL.AnyBackend qualified as AB
import Language.GraphQL.Draft.Syntax qualified as G

-------------------------------------------------------------------------------
-- Remote joins tree

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
-- similar in spirit to a GraphQL selection set in this regard.
--
-- This structure is somewhat similar to a prefix tree such as 'Data.Trie.Trie',
-- but has two additional guarantees:
--   - a 'JoinTree' is never empty,
--   - there cannot exist a pair of values for which one's prefix key is a
--     subset of the other: every value is effectively a leaf.
newtype JoinTree a = JoinTree {unJoinTree :: NEMap.NEHashMap QualifiedFieldName (JoinNode a)}
  deriving stock (Show, Eq, Functor, Foldable, Traversable, Generic)
  deriving newtype (Semigroup)

-- | A field name annotated with an optional type name.
--
-- To deal with ambiguous join paths, such as those that emerge from GraphQL
-- interfaces or GraphQL unions, we do not just keep track of the fields' name,
-- but also, optionally, of their type. Whenever a selection set is deemed
-- ambiguous, we insert a reserved field in the query to retrieve the typename,
-- @__hasura_internal_typename@; when traversing the join tree, if that key is
-- present, then we use it alongside the field name when querying the join tree
-- (see @traverseObject@ in the @Join@ module).
--
-- We use 'Text' for the representation of the field name instead of
-- 'FieldName', for simplicity: the join tree is only meant to be queried using
-- the values we get in the reponse, which will be unrestricted text.
data QualifiedFieldName = QualifiedFieldName
  { _qfTypeName :: Maybe Text,
    _qfFieldName :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

-- | Each leaf associates a mapping from typename to actual join info.
-- This allows to disambiguate between different remote joins with the same name
-- in a given selection set, which might happen with union or interface
-- fragments.
data JoinNode a
  = Leaf a
  | Tree (JoinTree a)
  deriving stock (Eq, Foldable, Functor, Generic, Traversable, Show)

type RemoteJoins = JoinTree RemoteJoin

-- | Collect all the remote joins to a remote schema from a join tree.
getRemoteSchemaJoins :: RemoteJoins -> [RemoteSchemaJoin]
getRemoteSchemaJoins = concatMap getRemoteSchemaJoin . toList
  where
    getRemoteSchemaJoin :: RemoteJoin -> [RemoteSchemaJoin]
    getRemoteSchemaJoin = \case
      RemoteJoinSource _ remoteJoins -> maybe [] getRemoteSchemaJoins remoteJoins
      RemoteJoinRemoteSchema s remoteJoins -> s : maybe [] getRemoteSchemaJoins remoteJoins

-------------------------------------------------------------------------------
-- Individual join information

-- | An individual join entry point in a 'JoinTree'.
--
-- Either a join against a source, or against a remote schema. In either case,
-- the constructor will contain that particular join's information (a
-- 'RemoteSourceJoin' or 'RemoteSchemaJoin' respectively) and, recursively, the
-- set of follow-up 'RemoteJoins' from that target, if any.
data RemoteJoin
  = RemoteJoinSource (AB.AnyBackend RemoteSourceJoin) (Maybe RemoteJoins)
  | RemoteJoinRemoteSchema RemoteSchemaJoin (Maybe RemoteJoins)
  deriving stock (Eq, Generic)

-- | A unique id that gets assigned to each 'RemoteJoin' (this is to avoid the
-- requirement of Ord/Hashable implementation for RemoteJoin)
type JoinCallId = Int

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

-- | Extracts the field name from the 'JoinColumnAlias', regardless of whether
-- the field is requested by the user of a "phantom" field.
getAliasFieldName :: JoinColumnAlias -> FieldName
getAliasFieldName = \case
  JCSelected f -> f
  JCPhantom f -> f

-- | Extracts the list of phantom field names out of a given 'RemoteJoin',
-- i.e. the name of the fields that must be part of the query but were not
-- requested by the user.
getPhantomFields :: RemoteJoin -> [FieldName]
getPhantomFields =
  mapMaybe getPhantomFieldName . HashMap.elems . getJoinColumnMapping
  where
    getPhantomFieldName :: JoinColumnAlias -> Maybe FieldName
    getPhantomFieldName = \case
      JCSelected _ -> Nothing
      JCPhantom f -> Just f

-- | Extracts an abstracted field mapping for a particular 'RemoteJoin', using a
-- common representation.
--
-- The RHS of the mapping uses 'JoinColumnAlias' instead of 'FieldName' to
-- differentiate between selected fields and phantom fields (see
-- 'JoinColumnAlias').
getJoinColumnMapping :: RemoteJoin -> HashMap.HashMap FieldName JoinColumnAlias
getJoinColumnMapping = \case
  RemoteJoinSource sourceJoin _ -> AB.runBackend
    sourceJoin
    \RemoteSourceJoin {_rsjJoinColumns} ->
      fmap fst _rsjJoinColumns
  RemoteJoinRemoteSchema RemoteSchemaJoin {_rsjJoinColumnAliases} _ ->
    _rsjJoinColumnAliases

-------------------------------------------------------------------------------
-- Join to source

-- | A 'RemoteSourceJoin' contains all the contextual information required for
-- the execution of a join against a source, translated from the IR's
-- representation of a selection (see 'AnnFieldG').
data RemoteSourceJoin b = RemoteSourceJoin
  { _rsjSource :: !SourceName,
    _rsjSourceConfig :: !(SourceConfig b),
    _rsjRelationship :: !(IR.SourceRelationshipSelection b Void IR.UnpreparedValue),
    _rsjJoinColumns :: !(HashMap.HashMap FieldName (JoinColumnAlias, (Column b, ScalarType b))),
    _rsjStringifyNum :: Options.StringifyNumbers
  }
  deriving (Generic)

deriving instance
  ( Backend b,
    Show (IR.SourceRelationshipSelection b Void IR.UnpreparedValue),
    Show (SourceConfig b)
  ) =>
  Show (RemoteSourceJoin b)

deriving instance
  ( Backend b,
    Eq (IR.SourceRelationshipSelection b Void IR.UnpreparedValue)
  ) =>
  Eq (RemoteSourceJoin b)

-------------------------------------------------------------------------------
-- Join to schema

-- | A 'RemoteSchemaJoin' contains all the contextual information required for
-- the execution of a join against a remote schema, translated from the IR's
-- representation of a selection (see 'AnnFieldG').
data RemoteSchemaJoin = RemoteSchemaJoin
  { -- | User-provided arguments with variables.
    _rsjArgs :: !(HashMap.HashMap G.Name (P.InputValue RemoteSchemaVariable)),
    -- | Customizer for JSON result from the remote server.
    _rsjResultCustomizer :: !ResultCustomizer,
    -- | User-provided selection set of remote field.
    _rsjSelSet :: !(IR.SelectionSet Void RemoteSchemaVariable),
    -- | A map of the join column to its alias in the response
    _rsjJoinColumnAliases :: !(HashMap.HashMap FieldName JoinColumnAlias),
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

-------------------------------------------------------------------------------
-- Join arguments

-- | A map of fieldname to values extracted from each LHS row/object
--
-- For example, if a remote relationship 'weather' on 'city' table
-- is defined as follows:
--   city.weather = get_weather(city: city.code, cityState: city.state_code)
-- a join argument for this join would have the values of columns 'code' and
-- 'state_code' for each 'city' row that participates in the join
newtype JoinArgument = JoinArgument {unJoinArgument :: HashMap.HashMap FieldName AO.Value}
  deriving stock (Eq, Generic, Show)
  deriving newtype (Hashable)

-- | A unique id assigned to each join argument
type JoinArgumentId = Int

data JoinArguments = JoinArguments
  { -- | The 'RemoteJoin' associated with the join arguments within this
    -- structure.
    _jalJoin :: !RemoteJoin,
    -- | Arguments for which we must fetch a response from the remote, along with
    -- the identifiers that are used to stitch the final response together.
    --
    -- NOTE: 'HashMap.HashMap' is used to deduplicate multiple 'JoinArgument's so that
    -- we avoid fetching more data from a remote than is necessary (i.e. in the
    -- case of duplicate arguments).
    _jalArguments :: !(HashMap.HashMap JoinArgument JoinArgumentId),
    -- | The 'FieldName' associated with the "replacement token" for this join
    -- argument.
    --
    -- NOTE: We need this for query logging; ideally we would use the full path
    -- for the GraphQL query associated with this remote join, but we don't have
    -- access to that here so this is the next best thing to do.
    _jalFieldName :: !FieldName
  }
  deriving stock (Generic)
