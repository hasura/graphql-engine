{-# LANGUAGE UndecidableInstances #-}
module Hasura.GraphQL.Execute.RemoteJoin.Types
  ( RemoteJoin(..)
  , getPhantomFields
  , getJoinColumnMapping
  -- , getRemoteSchemaJoin
  , getRemoteSchemaJoins
  , RemoteSchemaJoin(..)
  , RemoteSourceJoin(..)
  , RemoteJoins
  , JoinColumnAlias(..)
  , getAliasFieldName
  , getPhantomFieldName
  , JoinTree
  , JoinNode(..)
  , getLeaves

  , JoinCallId(..)
  , JoinArgumentId(..)
  , JoinArgument(..)
  , JoinArguments(..)
  , ReplacementToken
  , ResponsePath
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                    as A
import qualified Data.Aeson.Ordered            as AO
import qualified Data.HashMap.Strict           as Map
import qualified Data.List.NonEmpty            as NE
import qualified Language.GraphQL.Draft.Syntax as G

import           Data.Hashable                 (hashWithSalt)

import qualified Hasura.GraphQL.Parser         as P
import qualified Hasura.RQL.IR.Select          as IR
import qualified Hasura.SQL.AnyBackend         as AB

import           Hasura.RQL.Types

type JoinTree a = NE.NonEmpty (FieldName, (JoinNode a))

data JoinNode a
  = Leaf a
  | Tree !(JoinTree a)
  deriving (Show, Eq, Functor, Foldable, Traversable)

getLeaves :: JoinTree a -> [(FieldName, a)]
getLeaves joinTree =
  flip mapMaybe (toList joinTree) $
    \(fieldName, node) ->
      case node of
        Leaf a -> Just (fieldName, a)
        _      -> Nothing

type RemoteJoins = JoinTree RemoteJoin
-- type RemoteJoin = RemoteSchemaJoin

data RemoteJoin
  = RemoteJoinSource !(AB.AnyBackend RemoteSourceJoin)
  | RemoteJoinRemoteSchema !RemoteSchemaJoin
  deriving (Eq, Generic)

-- TODO: show doesn't get automatically derived for some reason

instance Hashable RemoteJoin where
  hashWithSalt = undefined

getRemoteSchemaJoin :: RemoteJoin -> Maybe RemoteSchemaJoin
getRemoteSchemaJoin = \case
  RemoteJoinSource _ -> Nothing
  RemoteJoinRemoteSchema s -> Just s

getRemoteSchemaJoins :: RemoteJoins -> [RemoteSchemaJoin]
getRemoteSchemaJoins =
  concatMap (mapMaybe getRemoteSchemaJoin . toList . snd)

-- instance Show RemoteJoinX where
--   show = \case
    -- RemoteJoinSource j ->
    --   AB.dispatchAnyBackend @Backend j (\x -> show x)
    -- RemoteJoinRemoteSchema j -> show j

-- getPhantomFields :: RemoteSchemaJoin -> [FieldName]
-- getPhantomFields remoteSchemaJoin =
--     mapMaybe getPhantomFieldName $ Map.elems $
--       _rsjJoinColumnAliases remoteSchemaJoin
--
getPhantomFields :: RemoteJoin -> [FieldName]
getPhantomFields =
  mapMaybe getPhantomFieldName . Map.elems . getJoinColumnMapping

getJoinColumnMapping :: RemoteJoin -> Map.HashMap FieldName JoinColumnAlias
getJoinColumnMapping remoteJoin =
  case remoteJoin of
  RemoteJoinSource sourceJoin ->
    AB.runBackend sourceJoin (fmap fst . _rdjJoinColumns)
  RemoteJoinRemoteSchema remoteSchemaJoin ->
    _rsjJoinColumnAliases remoteSchemaJoin
-- instance Show RemoteJoin where
--   show = \case
--     -- TODO: we'll need to start deriving show and eq instances all the way
--     -- from select ir
--     RemoteJoinSource j -> "remote join source"
--     RemoteJoinRemoteSchema j -> show j

-- -- TODO: fix this instance
-- instance Eq RemoteJoin where
--   j1 == j2 =
--     case (j1, j2) of
--       (RemoteJoinRemoteSchema j1, RemoteJoinRemoteSchema j2) -> j1 == j2
--       _ -> False


-- deriving instance
--   ( Backend b
--   , Eq (ScalarValue b)
--   , Eq (BooleanOperators b (P.UnpreparedValue b))
--   ) => Eq (RemoteSourceRelationship b)

-- deriving instance
--   ( Backend b
--   , Show (ScalarValue b)
--   , Show (BooleanOperators b (P.UnpreparedValue b))
--   ) => Show (RemoteSourceRelationship b)

-- | We need an Eq instance on RemoteSourceJoin b to group the values that are
-- going to the same source.
data RemoteSourceJoin b
  = RemoteSourceJoin
  { _rdjSource       :: !SourceName
  , _rdjSourceConfig :: !(SourceConfig b)
  , _rdjRelationship :: !(IR.SourceRelationshipSelection b (Const Void) P.UnpreparedValue)
  , _rdjJoinColumns  :: !(Map.HashMap FieldName (JoinColumnAlias, ScalarType b))
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

-- TODO: fix this instance
-- instance (Backend b, Hashable (SourceConfig b)) => Hashable (RemoteSourceJoin b)
  -- hashWithSalt x = undefined

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

newtype JoinCallId
  = JoinCallId { unJoinCallId :: Int }
  deriving (Show, Eq, Generic, Hashable, A.ToJSON)

newtype JoinArgumentId
  = JoinArgumentId { unArgumentId :: Int }
  deriving (Show, Eq, Generic, Hashable, A.ToJSON)

newtype JoinArgument
  = JoinArgument { unJoinArugment :: (Map.HashMap FieldName AO.Value) }
  deriving (Show, Eq, Generic, Hashable)

type ReplacementToken = (JoinCallId, JoinArgumentId)

data JoinArguments
  = JoinArguments
  { _jalJoinCallId :: !JoinCallId
  , _jalArguments  :: !(Map.HashMap JoinArgument JoinArgumentId)
  } deriving (Show, Eq, Generic)

type ResponsePath = NE.NonEmpty G.Name
