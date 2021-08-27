{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.GraphQL.Execute.RemoteJoin.Types
  ( RemoteJoin(..)
  , eqRemoteJoin
  , getPhantomFields
  , getJoinColumnMapping
  , getRemoteSchemaJoins
  , RemoteSchemaJoin(..)
  , eqRemoteSchemaJoin
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
  ) where

import           Hasura.Prelude

import           Data.Semigroup                (All (..))

import qualified Data.Aeson.Ordered            as AO
import qualified Data.HashMap.Strict           as Map
import qualified Data.IntMap.Strict            as IntMap
import qualified Data.List.NonEmpty            as NE
import qualified Language.GraphQL.Draft.Syntax as G

import qualified Hasura.GraphQL.Parser         as P

import           Hasura.RQL.Types

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
--
newtype JoinTree a =
  -- Ideally this should be represented as 'NonEmptyMap FieldName (JoinNode a)'
  -- if there is a good package which implements the above type
  JoinTree { unJoinTree :: NE.NonEmpty (FieldName, JoinNode a) }
  deriving stock (Eq, Foldable, Functor, Generic, Traversable, Show)
  deriving newtype (Semigroup)

data JoinNode a
  = Leaf a
  | Tree !(JoinTree a)
  deriving stock (Eq, Foldable, Functor, Generic, Traversable, Show)

type RemoteJoins = JoinTree RemoteJoin

-- | Currently a remote join is only possible to a remote schema, but this
-- should change soon when we merge the last of generalized joins work
newtype RemoteJoin
  = RemoteJoinRemoteSchema RemoteSchemaJoin
  deriving stock (Generic)

-- | Ad-hoc equality check for 'RemoteJoin', necessary due to the fact that
-- 'RemoteSchemaJoin' contains fields that do not support equality checks.
eqRemoteJoin :: RemoteJoin -> RemoteJoin -> Bool
eqRemoteJoin (RemoteJoinRemoteSchema a) (RemoteJoinRemoteSchema b) =
  a `eqRemoteSchemaJoin` b

-- | This collects all the remote joins from a join tree
getRemoteSchemaJoins :: RemoteJoins -> [RemoteSchemaJoin]
getRemoteSchemaJoins = map getRemoteSchemaJoin . toList
  where
    getRemoteSchemaJoin :: RemoteJoin -> RemoteSchemaJoin
    getRemoteSchemaJoin (RemoteJoinRemoteSchema s) = s

getPhantomFields :: RemoteJoin -> [FieldName]
getPhantomFields =
  mapMaybe getPhantomFieldName . Map.elems . getJoinColumnMapping
  where
    getPhantomFieldName :: JoinColumnAlias -> Maybe FieldName
    getPhantomFieldName = \case
      JCSelected _ -> Nothing
      JCPhantom f  -> Just f

getJoinColumnMapping :: RemoteJoin -> Map.HashMap FieldName JoinColumnAlias
getJoinColumnMapping = \case
  RemoteJoinRemoteSchema remoteSchemaJoin ->
    _rsjJoinColumnAliases remoteSchemaJoin

-- | Disambiguates between 'FieldName's which are provided as part of the
-- GraphQL selection provided by the user (i.e. 'JCSelected') and those which
-- we need to retreive data but which are not expressly requested (i.e.
-- 'JCPhantom').
--
-- After processing the remote join, we remove all phantom 'FieldName's and
-- only return those which fall under the 'JCSelected' branch of this type.
data JoinColumnAlias
  = JCSelected !FieldName
  -- ^ This fieldname is already part of the response.
  | JCPhantom !FieldName
  -- ^ This is explicitly added for the join.
  --
  -- Such keys will have to be removed from the response eventually.
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable)

getAliasFieldName :: JoinColumnAlias -> FieldName
getAliasFieldName = \case
  JCSelected f -> f
  JCPhantom f  -> f

-- | A 'RemoteJoin' represents the context of remote relationship to be
-- extracted from 'AnnFieldG's.
data RemoteSchemaJoin
  = RemoteSchemaJoin
  { _rsjArgs              :: !(Map.HashMap G.Name (P.InputValue RemoteSchemaVariable))
  -- ^ User-provided arguments with variables.
  , _rsjResultCustomizer  :: !RemoteResultCustomizer
  -- ^ Customizer for JSON result from the remote server
  , _rsjSelSet            :: !(G.SelectionSet G.NoFragments RemoteSchemaVariable)
    -- ^ User-provided selection set of remote field.
  , _rsjJoinColumnAliases :: !(Map.HashMap FieldName JoinColumnAlias)
    -- ^ A map of the join column to its alias in the response
  , _rsjFieldCall         :: !(NonEmpty FieldCall)
    -- ^ Remote server fields.
  , _rsjRemoteSchema      :: !RemoteSchemaInfo
    -- ^ The remote schema server info.
  } deriving stock (Generic)

-- | Ad-hoc equality check for 'RemoteSchemaJoin', performed only against the
-- fields that admit a valid equality check (i.e. not '_rsjResultCustomizer').
eqRemoteSchemaJoin :: RemoteSchemaJoin -> RemoteSchemaJoin -> Bool
eqRemoteSchemaJoin a b = getAll $ foldMap All
  [ _rsjArgs a              == _rsjArgs b
  , _rsjSelSet a            == _rsjSelSet b
  , _rsjJoinColumnAliases a == _rsjJoinColumnAliases b
  , _rsjFieldCall a         == _rsjFieldCall b
  , _rsjRemoteSchema a      == _rsjRemoteSchema b
  ]

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
newtype JoinArgument
  = JoinArgument { unJoinArugment :: Map.HashMap FieldName AO.Value }
  deriving stock (Eq, Generic, Show)
  deriving newtype (Hashable)

-- | A unique id assigned to each join argument
type JoinArgumentId = Int

-- | A map of JoinArgumentId to its value fetched from the RHS source of a join
type JoinIndex = IntMap.IntMap AO.Value

data JoinArguments
  = JoinArguments
  { _jalJoin      :: !RemoteJoin
  -- ^ The 'RemoteJoin' associated with the join arguments within this
  -- structure.
  , _jalArguments :: !(Map.HashMap JoinArgument JoinArgumentId)
  -- ^ Arguments for which we must fetch a response from the remote, along with
  -- the identifiers that are used to stitch the final response together.
  --
  -- NOTE: 'Map.HashMap' is used to deduplicate multiple 'JoinArgument's so that
  -- we avoid fetching more data from a remote than is necessary (i.e. in the
  -- case of duplicate arguments).
  } deriving stock (Generic)
