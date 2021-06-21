{-# LANGUAGE UndecidableInstances #-}
module Hasura.GraphQL.Execute.RemoteJoin.Types
  ( RemoteJoin(..)
  , getPhantomFields
  , getJoinColumnMapping
  , getRemoteSchemaJoins
  , RemoteSchemaJoin(..)
  , RemoteSourceJoin(..)
  , RemoteJoins
  , JoinColumnAlias(..)
  , getAliasFieldName

  , JoinTree(..)
  , JoinNode(..)
  , JoinCallId
  , JoinArgumentId
  , JoinArgument(..)
  , JoinIndex
  , JoinArguments(..)
  , ReplacementToken
  , ResponsePath
  ) where

import           Hasura.Prelude

import qualified Data.Aeson.Ordered            as AO
import qualified Data.HashMap.Strict           as Map
import qualified Data.IntMap.Strict            as IntMap
import qualified Data.List.NonEmpty            as NE
import qualified Language.GraphQL.Draft.Syntax as G

import           Control.Lens

import qualified Hasura.GraphQL.Parser         as P
import qualified Hasura.RQL.IR.Select          as IR
import qualified Hasura.SQL.AnyBackend         as AB

import           Hasura.RQL.Types

newtype JoinTree a =
  JoinTree { unJoinTree :: NE.NonEmpty (FieldName, (JoinNode a)) }
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

data JoinNode a
  = Leaf a
  | Tree !(JoinTree a)
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

type RemoteJoins = JoinTree RemoteJoin

data RemoteJoin
  = RemoteJoinSource !(AB.AnyBackend RemoteSourceJoin) !(Maybe RemoteJoins)
  | RemoteJoinRemoteSchema !RemoteSchemaJoin
  deriving (Eq, Generic)
  -- deriving (Eq, Show, Generic)

getRemoteSchemaJoin :: RemoteJoin -> Maybe RemoteSchemaJoin
getRemoteSchemaJoin = \case
  RemoteJoinSource _ _     -> Nothing
  RemoteJoinRemoteSchema s -> Just s

getRemoteSchemaJoins :: RemoteJoins -> [RemoteSchemaJoin]
getRemoteSchemaJoins =
  mapMaybe getRemoteSchemaJoin . toList

getPhantomFields :: RemoteJoin -> [FieldName]
getPhantomFields =
  mapMaybe getPhantomFieldName . Map.elems . getJoinColumnMapping

getJoinColumnMapping :: RemoteJoin -> Map.HashMap FieldName JoinColumnAlias
getJoinColumnMapping remoteJoin =
  case remoteJoin of
  RemoteJoinSource sourceJoin _ ->
    AB.runBackend sourceJoin (fmap (^. _1) . _rdjJoinColumns)
  RemoteJoinRemoteSchema remoteSchemaJoin ->
    _rsjJoinColumnAliases remoteSchemaJoin

-- instance Show RemoteJoin where
--   show = \case
--     -- TODO: we'll need to start deriving show and eq instances all the way
--     -- from select ir
--     RemoteJoinSource j -> "remote join source"
--     RemoteJoinRemoteSchema j -> show j

-- | We need an Eq instance on RemoteSourceJoin b to group the values that are
-- going to the same source.
data RemoteSourceJoin b
  = RemoteSourceJoin
  { _rdjSource       :: !SourceName
  , _rdjSourceConfig :: !(SourceConfig b)
  , _rdjRelationship :: !(IR.SourceRelationshipSelection b (Const Void) P.UnpreparedValue)
  , _rdjJoinColumns  :: !(Map.HashMap FieldName (JoinColumnAlias, ScalarType b, Column b))
  } deriving (Generic)

deriving instance
  ( Backend b
  , Show (ScalarValue b)
  , Show (SourceConfig b)
  , Show (BooleanOperators b (P.UnpreparedValue b))
  ) => Show (RemoteSourceJoin b)

deriving instance
  ( Backend b
  , Eq (ScalarValue b)
  , Eq (BooleanOperators b (P.UnpreparedValue b))
  ) => Eq (RemoteSourceJoin b)

-- | A 'RemoteJoin' represents the context of remote relationship to be
-- extracted from 'AnnFieldG's.
data RemoteSchemaJoin
  = RemoteSchemaJoin
  { _rsjArgs              :: !(Map.HashMap G.Name (P.InputValue RemoteSchemaVariable))
    -- ^ User-provided arguments with variables.
  , _rsjSelSet            :: !(G.SelectionSet G.NoFragments RemoteSchemaVariable)
    -- ^ User-provided selection set of remote field.
  , _rsjJoinColumnAliases :: !(Map.HashMap FieldName JoinColumnAlias)
    -- ^ A map of the join column to its alias in the response
  , _rsjFieldCall         :: !(NonEmpty FieldCall)
    -- ^ Remote server fields.
  , _rsjRemoteSchema      :: !RemoteSchemaInfo
    -- ^ The remote schema server info.
  } deriving (Show, Eq, Generic)

instance Hashable RemoteSchemaJoin

data JoinColumnAlias
  = JCSelected !FieldName
  -- ^ This fieldname is already part of the response
  | JCPhantom !FieldName
  -- ^ This is explicitly added for the join. Such
  -- keys will have to be removed from the response eventually
  deriving (Show, Eq, Generic)

instance Hashable JoinColumnAlias

getPhantomFieldName :: JoinColumnAlias -> Maybe FieldName
getPhantomFieldName = \case
  JCSelected _ -> Nothing
  JCPhantom f  -> Just f

getAliasFieldName :: JoinColumnAlias -> FieldName
getAliasFieldName = \case
  JCSelected f -> f
  JCPhantom f  -> f

type JoinCallId = Int
type JoinArgumentId = Int

newtype JoinArgument
  = JoinArgument { unJoinArugment :: (Map.HashMap FieldName AO.Value) }
  deriving (Show, Eq, Generic, Hashable)

type JoinIndex = IntMap.IntMap AO.Value

type ReplacementToken = (JoinCallId, JoinArgumentId)

data JoinArguments
  = JoinArguments
  { _jalJoin       :: !RemoteJoin
  , _jalArguments  :: !(Map.HashMap JoinArgument JoinArgumentId)
  } deriving (Eq, Generic)

type ResponsePath = NE.NonEmpty G.Name
