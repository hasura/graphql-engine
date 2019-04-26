{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hasura.RQL.Types.QueryCollection where

import           Hasura.GraphQL.Validate.Types (stripeOffTypeNames)
import           Hasura.Prelude
import           Hasura.RQL.Types.Error
import           Hasura.SQL.Types

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax    (Lift)

import qualified Data.HashMap.Strict           as HM
import qualified Data.HashSet                  as HS
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
  deriving (Show, Eq, Ord, Hashable, Lift, ToJSON, ToJSONKey, FromJSON, DQuote)

newtype GQLQuery a
  = GQLQuery {unGQLQuery :: a}
  deriving (Show, Eq, Hashable, Lift, ToJSON, FromJSON)

type GQLQueryParsed = GQLQuery G.ExecutableDocument

instance ToJSON G.ExecutableDocument where
  -- TODO:- Define proper toJSON instance using GraphQL query printer
  toJSON _ = String "toJSON for Executable Document is not implemented yet"

data WhitelistedQuery a
  = WhitelistedQuery
  { _wlqName  :: !QueryName
  , _wlqQuery :: !(GQLQuery a)
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 4 snakeCase) ''WhitelistedQuery)

toParsedWhitelistedQuery
  :: MonadError QErr m
  => WhitelistedQuery T.Text -> m (WhitelistedQuery G.ExecutableDocument)
toParsedWhitelistedQuery (WhitelistedQuery name (GQLQuery t)) =
  case G.parseExecutableDoc t of
    Left _ -> throw400 ParseFailed $
              "parsing graphql query with name " <> name <<> " failed"
    Right a -> return $ WhitelistedQuery name $ GQLQuery $ G.ExecutableDocument
               $ stripeOffTypeNames $ G.getExecutableDefinitions a

type QueryList a = [WhitelistedQuery a]
type ParsedQueryList = QueryList G.ExecutableDocument

newtype CollectionDef a
  = CollectionDef
  { _cdQueries :: QueryList a}
  deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 3 snakeCase) ''CollectionDef)

data CreateCollection
  = CreateCollection
  { _ccName       :: !CollectionName
  , _ccDefinition :: !(CollectionDef T.Text)
  , _ccComment    :: !(Maybe T.Text)
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 3 snakeCase) ''CreateCollection)

type QueryMap = HM.HashMap QueryName GQLQueryParsed
type CollectionMap = HM.HashMap CollectionName QueryMap

allWhitelistedQueries :: CollectionMap -> [GQLQueryParsed]
allWhitelistedQueries =
  HS.toList . HS.fromList . concatMap HM.elems . HM.elems

queryListToMap :: ParsedQueryList  -> QueryMap
queryListToMap ql =
  HM.fromList $ flip map ql $ \(WhitelistedQuery n q) -> (n, q)

newtype DropCollection
  = DropCollection
  { _dcName :: CollectionName}
  deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 3 snakeCase) ''DropCollection)

data AddQueryToCollection
  = AddQueryToCollection
  { _aqtcCollectionName :: !CollectionName
  , _aqtcQueryName      :: !QueryName
  , _aqtcQuery          :: !T.Text
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 5 snakeCase) ''AddQueryToCollection)

data DropQueryFromCollection
  = DropQueryFromCollection
  { _dqfcCollectionName :: !CollectionName
  , _dqfcQueryName      :: !QueryName
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 5 snakeCase) ''DropQueryFromCollection)
