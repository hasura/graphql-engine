module Hasura.RQL.Types.QueryCollection
  ( CollectionName(..)
  , QueryName(..)
  , GQLQuery(..)
  , WhitelistedQuery(..)
  , QueryList
  , CollectionDef(..)
  , CreateCollection(..)
  , CollectionMap
  , allWhitelistedQueries
  , queryListToMap
  , DropCollection(..)
  ) where

import           Hasura.GraphQL.Validate.Types (stripeOffTypeNames)
import           Hasura.Prelude
import           Hasura.SQL.Types

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax    (Lift)

import qualified Data.HashMap.Strict           as HM
import qualified Data.HashSet                  as HS
import qualified Data.List.NonEmpty            as NE
import qualified Data.Text                     as T
import qualified Database.PG.Query             as Q
import qualified Language.GraphQL.Draft.Parser as G
import qualified Language.GraphQL.Draft.Syntax as G

newtype CollectionName
  = CollectionName {unCollectionName :: T.Text}
  deriving ( Show, Eq, Hashable, ToJSON, ToJSONKey, Lift
           , FromJSON, Q.FromCol, Q.ToPrepArg, DQuote
           )

newtype QueryName
  = QueryName {unQueryName :: T.Text}
  deriving (Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, FromJSON, DQuote)

newtype GQLQuery
  = GQLQuery {unGQLQuery :: G.ExecutableDocument}
  deriving (Show, Eq, Hashable)

instance FromJSON GQLQuery where
  parseJSON = withText "GQLQuery" $ \t ->
    case G.parseExecutableDoc t of
      Left _  -> fail "parsing the graphql query failed"
      Right a -> return $ GQLQuery $ G.ExecutableDocument $
                 stripeOffTypeNames $ G.getExecutableDefinitions a

instance ToJSON GQLQuery where
  toJSON _ = String "toJSON for GQLQuery is not implemented yet"

data WhitelistedQuery
  = WhitelistedQuery
  { _wlqName  :: !QueryName
  , _wlqQuery :: !GQLQuery
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 4 snakeCase) ''WhitelistedQuery)

type QueryList = NE.NonEmpty WhitelistedQuery

newtype CollectionDef
  = CollectionDef
  { _cdQueries :: QueryList}
  deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase) ''CollectionDef)

data CreateCollection
  = CreateCollection
  { _ccName       :: !CollectionName
  , _ccDefinition :: !Value
  , _ccComment    :: !(Maybe T.Text)
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 3 snakeCase) ''CreateCollection)

type QueryMap = HM.HashMap QueryName GQLQuery
type CollectionMap = HM.HashMap CollectionName QueryMap

allWhitelistedQueries :: CollectionMap -> [GQLQuery]
allWhitelistedQueries =
  HS.toList . HS.fromList . concatMap HM.elems . HM.elems

queryListToMap :: QueryList -> QueryMap
queryListToMap ql =
  HM.fromList $ flip map (toList ql) $ \(WhitelistedQuery n q) -> (n, q)

newtype DropCollection
  = DropCollection
  { _dcName :: CollectionName}
  deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 3 snakeCase) ''DropCollection)
