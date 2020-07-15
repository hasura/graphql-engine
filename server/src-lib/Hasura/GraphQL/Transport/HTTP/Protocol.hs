{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

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
  , GQRespValue(..), gqRespData, gqRespErrors
  , encodeGQRespValue
  , parseGQRespValue
  , parseEncJObject
  , GQJoinError(..), gqJoinErrorToValue
  ) where

import           Control.Lens
import           Hasura.EncJSON
import           Hasura.GraphQL.Utils
import           Hasura.Prelude
import           Hasura.RQL.Types

import           Language.GraphQL.Draft.Instances ()
import           Language.Haskell.TH.Syntax       (Lift)

import qualified Data.Aeson                       as J
import qualified Data.Aeson.Casing                as J
import qualified Data.Aeson.Ordered               as OJ
import qualified Data.Aeson.TH                    as J
import qualified Data.ByteString.Lazy             as BL
import qualified Data.HashMap.Strict              as Map
import qualified Data.Vector                      as V
import qualified Language.GraphQL.Draft.Parser    as G
import qualified Language.GraphQL.Draft.Syntax    as G
import qualified VectorBuilder.Builder            as VB
import qualified VectorBuilder.Vector             as VB

newtype GQLExecDoc
  = GQLExecDoc { unGQLExecDoc :: [G.ExecutableDefinition] }
  deriving (Ord, Show, Eq, Hashable, Lift)

instance J.FromJSON GQLExecDoc where
  parseJSON v = GQLExecDoc . G.getExecutableDefinitions <$> J.parseJSON v

instance J.ToJSON GQLExecDoc where
  toJSON = J.toJSON . G.ExecutableDocument . unGQLExecDoc

newtype OperationName
  = OperationName { _unOperationName :: G.Name }
  deriving (Ord, Show, Eq, Hashable, J.ToJSON, Lift)

instance J.FromJSON OperationName where
  parseJSON v = OperationName . G.Name <$> J.parseJSON v

type VariableValues = Map.HashMap G.Variable J.Value

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
  Left _  -> withPathK "query" $ throwVE "not a valid graphql query"
  Right a -> return $ req { _grQuery = GQLExecDoc $ G.getExecutableDefinitions a }
  where
    gqlText = _unGQLQueryText $ _grQuery req

encodeGQErr :: Bool -> QErr -> J.Value
encodeGQErr includeInternal qErr =
  J.object [ "errors" J..= [encodeGQLErr includeInternal qErr]]

-- | https://graphql.github.io/graphql-spec/June2018/#sec-Response-Format
--
-- NOTE: this type and parseGQRespValue are a lax representation of the spec,
-- since...
--   - remote GraphQL servers may not conform strictly, and...
--   - we use this type as an accumulator.
--
-- Ideally we'd have something correct by construction for hasura results
-- someplace.
data GQRespValue =
  GQRespValue
  { _gqRespData   :: OJ.Object
  -- ^ 'OJ.empty' (corresponding to the invalid `"data": {}`) indicates an error.
  , _gqRespErrors :: VB.Builder OJ.Value
  -- ^ An 'OJ.Array', but with efficient cons and concatenation. Null indicates
  -- query success.
  }

makeLenses ''GQRespValue

newtype GQJoinError =  GQJoinError Text
  deriving (Show, Eq, IsString, Monoid, Semigroup)

-- | https://graphql.github.io/graphql-spec/June2018/#sec-Errors  "Error result format"
gqJoinErrorToValue :: GQJoinError -> OJ.Value
gqJoinErrorToValue (GQJoinError msg) =
  OJ.Object (OJ.fromList [("message", OJ.String msg)])

data GQResult a
  = GQSuccess !a
  | GQPreExecError ![J.Value]
  | GQExecError ![J.Value]
  | GQGeneric  !GQRespValue
  deriving (Functor, Foldable, Traversable)

type GQResponse = GQResult BL.ByteString

isExecError :: GQResult a -> Bool
isExecError = \case
  GQExecError _ -> True
  _             -> False

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

-- emptyResp :: GQRespValue
-- emptyResp = GQRespValue OJ.empty VB.empty

parseEncJObject :: EncJSON -> Either String OJ.Object
parseEncJObject = OJ.eitherDecode . encJToLBS >=> \case
  OJ.Object obj -> pure obj
  _             -> Left "expected object for GraphQL response"

parseGQRespValue :: EncJSON -> Either String GQRespValue
parseGQRespValue = parseEncJObject >=> \obj -> do
  _gqRespData <-
    case OJ.lookup "data" obj of
      -- "an error was encountered before execution began":
      Nothing               -> pure OJ.empty
      -- "an error was encountered during the execution that prevented a valid response":
      Just OJ.Null          -> pure OJ.empty
      Just (OJ.Object dobj) -> pure dobj
      Just _                -> Left "expected object or null for GraphQL data response"
  _gqRespErrors <-
    case OJ.lookup "errors" obj of
      Nothing             -> pure VB.empty
      Just (OJ.Array vec) -> pure $ VB.vector vec
      Just _              -> Left "expected array for GraphQL error response"
  pure (GQRespValue {_gqRespData, _gqRespErrors})

encodeGQRespValue :: GQRespValue -> EncJSON
encodeGQRespValue GQRespValue{..} = OJ.toEncJSON $ OJ.Object $ OJ.fromList $
  -- "If the data entry in the response is not present, the errors entry in the
  -- response must not be empty. It must contain at least one error. "
  if _gqRespData == OJ.empty && not anyErrors
    then
      let msg = "Somehow did not accumulate any errors or data from graphql queries"
       in [("errors", OJ.Array $ V.singleton $ OJ.Object (OJ.fromList [("message", OJ.String msg)]) )]
    else
      -- NOTE: "If an error was encountered during the execution that prevented
      -- a valid response, the data entry in the response should be null."
      -- TODO it's not clear to me how we can enforce that here or if we should try.
      ("data", OJ.Object _gqRespData) :
      [("errors", OJ.Array gqRespErrorsV) | anyErrors ]
  where
    gqRespErrorsV = VB.build _gqRespErrors
    anyErrors = not $ V.null gqRespErrorsV

encodeGQResp :: GQResponse -> EncJSON
encodeGQResp = \case
  GQSuccess r      -> encJFromAssocList [("data", encJFromLBS r)]
  GQPreExecError e -> encJFromAssocList [("errors", encJFromJValue e)]
  GQExecError e    -> encJFromAssocList [("data", "null"), ("errors", encJFromJValue e)]
  GQGeneric v -> encodeGQRespValue v
