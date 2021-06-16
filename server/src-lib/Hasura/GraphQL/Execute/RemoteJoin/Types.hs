module Hasura.GraphQL.Execute.RemoteJoin.Types
  ( RemoteJoin(..)
  , RemoteJoins
  , JoinColumnAlias(..)
  , getAliasFieldName
  , getPhantomFieldName
  , JoinTree
  , JoinNode(..)
  , getLeaves
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict           as Map
import qualified Data.List.NonEmpty            as NE
import qualified Language.GraphQL.Draft.Syntax as G

import qualified Hasura.GraphQL.Parser         as P

import           Hasura.RQL.Types

-- | A 'RemoteJoin' represents the context of remote relationship to be
-- extracted from 'AnnFieldG's.
data RemoteJoin
  = RemoteJoin
  { _rjArgs              :: !(Map.HashMap G.Name (P.InputValue RemoteSchemaVariable)) -- ^ User-provided arguments with variables.
  , _rjSelSet            :: !(G.SelectionSet G.NoFragments RemoteSchemaVariable)  -- ^ User-provided selection set of remote field.
  , _rjJoinColumnAliases :: !(Map.HashMap FieldName JoinColumnAlias)
  -- ^ A map of the join column to its alias in the response
  , _rjFieldCall         :: !(NonEmpty FieldCall) -- ^ Remote server fields.
  , _rjRemoteSchema      :: !RemoteSchemaInfo -- ^ The remote schema server info.
  } deriving (Show, Eq, Generic)

instance Hashable RemoteJoin

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
