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
  , GQResult
  , GQExecError(..)
  , GQResponse
  , isExecError
  ) where

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.Types

import           Data.Either                   (isLeft)
import           Language.Haskell.TH.Syntax    (Lift)

import qualified Data.Aeson                    as J
import qualified Data.Aeson.Casing             as J
import qualified Data.Aeson.TH                 as J
import qualified Data.ByteString.Lazy          as BL
import qualified Data.HashMap.Strict           as Map
import qualified Language.GraphQL.Draft.Parser as G
import qualified Language.GraphQL.Draft.Syntax as G

newtype GQLExecDoc
  = GQLExecDoc { unGQLExecDoc :: [G.ExecutableDefinition G.Name] }
  deriving (Ord, Show, Eq, Hashable,Lift)

instance J.FromJSON GQLExecDoc where
  parseJSON v = GQLExecDoc . G.getExecutableDefinitions <$> J.parseJSON v

instance J.ToJSON GQLExecDoc where
  toJSON = J.toJSON . G.ExecutableDocument . unGQLExecDoc

newtype OperationName
  = OperationName { _unOperationName :: G.Name }
  deriving (Ord, Show, Eq, Hashable, J.ToJSON, Lift)

instance J.FromJSON OperationName where
  parseJSON v = OperationName <$> J.parseJSON v

type VariableValues = Map.HashMap G.Name J.Value

data GQLReq a
  = GQLReq
  { _grOperationName :: !(Maybe OperationName)
  , _grQuery         :: !a
  , _grVariables     :: !(Maybe VariableValues)
  } deriving (Show, Eq, Generic, Functor, Lift)

$(J.deriveJSON (J.aesonDrop 3 J.camelCase){J.omitNothingFields=True} ''GQLReq)

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
  } deriving (Show, Eq, Ord, J.FromJSON, J.ToJSON, Hashable, Lift, IsString)

type GQLReqUnparsed = GQLReq GQLQueryText
type GQLReqParsed = GQLReq GQLExecDoc

toParsed :: (MonadError QErr m ) => GQLReqUnparsed -> m GQLReqParsed
toParsed req = case G.parseExecutableDoc gqlText of
  Left _  -> withPathK "query" $ throw400 ValidationFailed "not a valid graphql query"
  Right a -> return $ req { _grQuery = GQLExecDoc $ G.getExecutableDefinitions a }
  where
    gqlText = _unGQLQueryText $ _grQuery req

encodeGQErr :: Bool -> QErr -> J.Value
encodeGQErr includeInternal qErr =
  J.object [ "errors" J..= [encodeGQLErr includeInternal qErr]]

type GQResult a = Either GQExecError a

newtype GQExecError = GQExecError [J.Value]
  deriving (Show, Eq, J.ToJSON)

type GQResponse = GQResult BL.ByteString

isExecError :: GQResult a -> Bool
isExecError = isLeft

encodeGQResp :: GQResponse -> EncJSON
encodeGQResp gqResp =
  encJFromAssocList $ case gqResp of
    Right r -> [("data", encJFromLBS r)]
    Left e  -> [("data", "null"), ("errors", encJFromJValue e)]
