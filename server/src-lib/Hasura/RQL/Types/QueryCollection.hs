module Hasura.RQL.Types.QueryCollection where

import           Hasura.GraphQL.Instances      ()
import           Hasura.GraphQL.Validate.Types (stripeOffTypeNames)
import           Hasura.Prelude
import           Hasura.SQL.Types

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax    (Lift)

import qualified Data.HashMap.Strict           as HM
import qualified Data.HashSet                  as HS
import qualified Data.Text                     as T
import qualified Database.PG.Query             as Q
import qualified Language.GraphQL.Draft.Syntax as G

newtype CollectionName
  = CollectionName {unCollectionName :: T.Text}
  deriving ( Show, Eq, Hashable, ToJSON, ToJSONKey, Lift
           , FromJSON, Q.FromCol, Q.ToPrepArg, DQuote
           )

newtype QueryName
  = QueryName {unQueryName :: T.Text}
  deriving (Show, Eq, Ord, Hashable, Lift, ToJSON, ToJSONKey, FromJSON, DQuote)

newtype GQLQuery
  = GQLQuery {unGQLQuery :: G.ExecutableDocument}
  deriving (Show, Eq, Hashable, Lift, ToJSON, FromJSON)

data ListedQuery
  = ListedQuery
  { _lqName  :: !QueryName
  , _lqQuery :: !GQLQuery
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 3 snakeCase) ''ListedQuery)

type QueryList = [ListedQuery]

newtype CollectionDef
  = CollectionDef
  { _cdQueries :: QueryList }
  deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 3 snakeCase) ''CollectionDef)

data CreateCollection
  = CreateCollection
  { _ccName       :: !CollectionName
  , _ccDefinition :: !CollectionDef
  , _ccComment    :: !(Maybe T.Text)
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 3 snakeCase) ''CreateCollection)

type QueryMap = HM.HashMap QueryName GQLQuery
type CollectionMap = HM.HashMap CollectionName QueryMap

allListedQueries :: CollectionMap -> [GQLQuery]
allListedQueries =
  HS.toList . HS.fromList . concatMap HM.elems . HM.elems

queryListToMap :: QueryList  -> QueryMap
queryListToMap ql =
  HM.fromList $ flip map ql $ \(ListedQuery queryName query) ->
    ( queryName
    -- remove __typename field
    , GQLQuery $ G.ExecutableDocument $
      stripeOffTypeNames $ G.getExecutableDefinitions $ unGQLQuery query
    )

newtype DropCollection
  = DropCollection
  { _dcName :: CollectionName}
  deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 3 snakeCase) ''DropCollection)

data AddQueryToCollection
  = AddQueryToCollection
  { _aqtcCollectionName :: !CollectionName
  , _aqtcQueryName      :: !QueryName
  , _aqtcQuery          :: !GQLQuery
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 5 snakeCase) ''AddQueryToCollection)

data DropQueryFromCollection
  = DropQueryFromCollection
  { _dqfcCollectionName :: !CollectionName
  , _dqfcQueryName      :: !QueryName
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 5 snakeCase) ''DropQueryFromCollection)
