module Hasura.GraphQL.Transport.HTTP.Protocol
  ( GQLReq(..)
  , GQLBatchedReqs(..)
  , GQLReqUnparsed
  , GQLReqParsed
  , toParsed
  , GQLQueryText(..)
  , GQLExecDoc(..)
  , OperationName(..)
  , VariableValues
  , encodeGQErr
  , encodeGQResp
  , GQResult(..)
  , GQResponse
  , isExecError
  , RemoteGqlResp(..)
  , GraphqlResponse(..)
  , encodeGraphqlResponse
  ) where

import           Hasura.EncJSON
import           Hasura.GraphQL.Utils
import           Hasura.Prelude
import           Hasura.RQL.Types

import           Language.GraphQL.Draft.Instances ()

import qualified Data.Aeson                       as J
import qualified Data.Aeson.Casing                as J
import qualified Data.Aeson.TH                    as J
import qualified Data.ByteString.Lazy             as BL
import qualified Data.HashMap.Strict              as Map
import qualified Language.GraphQL.Draft.Parser    as G
import qualified Language.GraphQL.Draft.Syntax    as G

newtype GQLExecDoc
  = GQLExecDoc { unGQLExecDoc :: [G.ExecutableDefinition] }
  deriving (Ord, Show, Eq, Hashable)

instance J.FromJSON GQLExecDoc where
  parseJSON v = (GQLExecDoc . G.getExecutableDefinitions) <$> J.parseJSON v

instance J.ToJSON GQLExecDoc where
  toJSON = J.toJSON . G.ExecutableDocument . unGQLExecDoc

newtype OperationName
  = OperationName { _unOperationName :: G.Name }
  deriving (Ord, Show, Eq, Hashable, J.ToJSON)

instance J.FromJSON OperationName where
  parseJSON v = OperationName . G.Name <$> J.parseJSON v

type VariableValues = Map.HashMap G.Variable J.Value

data GQLReq a
  = GQLReq
  { _grOperationName :: !(Maybe OperationName)
  , _grQuery         :: !a
  , _grVariables     :: !(Maybe VariableValues)
  } deriving (Show, Eq, Generic)

$(J.deriveJSON (J.aesonDrop 3 J.camelCase){J.omitNothingFields=True}
  ''GQLReq
 )

instance (Hashable a) => Hashable (GQLReq a)

-- | Batched queries are sent as a JSON array of
-- 'GQLReq' records. This newtype exists to support
-- the unusual JSON encoding.
--
-- See <https://github.com/hasura/graphql-engine/issues/1812>.
data GQLBatchedReqs a
  = GQLSingleRequest (GQLReq a)
  | GQLBatchedReqs [GQLReq a]
  deriving (Show, Eq, Generic)

instance J.ToJSON a => J.ToJSON (GQLBatchedReqs a) where
  toJSON (GQLSingleRequest q) = J.toJSON q
  toJSON (GQLBatchedReqs qs)  = J.toJSON qs

instance J.FromJSON a => J.FromJSON (GQLBatchedReqs a) where
  parseJSON arr@J.Array{} = GQLBatchedReqs <$> J.parseJSON arr
  parseJSON other         = GQLSingleRequest <$> J.parseJSON other

newtype GQLQueryText
  = GQLQueryText
  { _unGQLQueryText :: Text
  } deriving (Show, Eq, Ord, J.FromJSON, J.ToJSON, Hashable)

type GQLReqUnparsed = GQLReq GQLQueryText
type GQLReqParsed = GQLReq GQLExecDoc

toParsed :: (MonadError QErr m ) => GQLReqUnparsed -> m GQLReqParsed
toParsed req = case G.parseExecutableDoc gqlText of
  Left _  -> withPathK "query" $ throwVE "not a valid graphql query"
  Right a -> return $ req { _grQuery = GQLExecDoc $ G.getExecutableDefinitions a }
  where
    gqlText = _unGQLQueryText $ _grQuery req

encodeGQErr :: Bool -> QErr -> J.Value
encodeGQErr includeInternal qErr =
  J.object [ "errors" J..= [encodeGQLErr includeInternal qErr]]

data GQResult a
  = GQSuccess !a
  | GQPreExecError ![J.Value]
  | GQExecError ![J.Value]
  deriving (Show, Eq, Functor, Foldable, Traversable)

type GQResponse = GQResult BL.ByteString

isExecError :: GQResult a -> Bool
isExecError = \case
  GQExecError _ -> True
  _             -> False

encodeGQResp :: GQResponse -> EncJSON
encodeGQResp gqResp =
  encJFromAssocList $ case gqResp of
    GQSuccess r      -> [("data", encJFromLBS r)]
    GQPreExecError e -> [("errors", encJFromJValue e)]
    GQExecError e    -> [("data", "null"), ("errors", encJFromJValue e)]

-- | Represents GraphQL response from a remote server
data RemoteGqlResp
  = RemoteGqlResp
  { _rgqrData       :: !(Maybe J.Value)
  , _rgqrErrors     :: !(Maybe [J.Value])
  , _rgqrExtensions :: !(Maybe J.Value)
  } deriving (Show, Eq)
$(J.deriveFromJSON (J.aesonDrop 5 J.camelCase) ''RemoteGqlResp)

encodeRemoteGqlResp :: RemoteGqlResp -> EncJSON
encodeRemoteGqlResp (RemoteGqlResp d e ex) =
  encJFromAssocList [ ("data", encJFromJValue d)
                    , ("errors", encJFromJValue e)
                    , ("extensions", encJFromJValue ex)
                    ]

-- | Represents a proper GraphQL response
data GraphqlResponse
  = GRHasura !GQResponse
  | GRRemote !RemoteGqlResp

encodeGraphqlResponse :: GraphqlResponse -> EncJSON
encodeGraphqlResponse = \case
  GRHasura resp -> encodeGQResp resp
  GRRemote resp -> encodeRemoteGqlResp resp
