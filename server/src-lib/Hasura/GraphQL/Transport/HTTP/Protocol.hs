module Hasura.GraphQL.Transport.HTTP.Protocol
  ( GQLReq(..)
  , GQLReqUnparsed
  , GQLReqParsed
  , toParsed
  , GQLExecDoc(..)
  , OperationName(..)
  , VariableValues
  , encodeGQErr
  , encodeGQResp
  , GQResp(..)
  , isExecError
  ) where

import           Hasura.EncJSON
import           Hasura.GraphQL.Utils
import           Hasura.Prelude
import           Hasura.RQL.Types

import qualified Data.Aeson                    as J
import qualified Data.Aeson.Casing             as J
import qualified Data.Aeson.TH                 as J
import qualified Data.HashMap.Strict           as Map
import qualified Language.GraphQL.Draft.Parser as G
import qualified Language.GraphQL.Draft.Syntax as G

newtype GQLExecDoc
  = GQLExecDoc { unGQLExecDoc :: [G.ExecutableDefinition] }
  deriving (Ord, Show, Eq, Hashable)

instance J.FromJSON GQLExecDoc where
  parseJSON = J.withText "GQLExecDoc" $ \t ->
    case G.parseExecutableDoc t of
      Left _  -> fail "parsing the graphql query failed"
      Right q -> return $ GQLExecDoc $ G.getExecutableDefinitions q

instance J.ToJSON GQLExecDoc where
  -- TODO, add pretty printer in graphql-parser
  toJSON _ = J.String "toJSON not implemented for GQLExecDoc"

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

type GQLReqUnparsed = GQLReq Text
type GQLReqParsed = GQLReq GQLExecDoc

toParsed :: (MonadError QErr m ) => GQLReqUnparsed -> m GQLReqParsed
toParsed req = case G.parseExecutableDoc $ _grQuery req of
  Left _ -> withPathK "query" $ throwVE "not a valid graphql query"
  Right a -> return $ req { _grQuery = GQLExecDoc $ G.getExecutableDefinitions a }

encodeGQErr :: Bool -> QErr -> J.Value
encodeGQErr includeInternal qErr =
  J.object [ "errors" J..= [encodeGQLErr includeInternal qErr]]

data GQResp
  -- TODO: this should probably be BL.ByteString
  = GQSuccess !EncJSON
  | GQPreExecError ![J.Value]
  | GQExecError ![J.Value]

isExecError :: GQResp -> Bool
isExecError = \case
  GQExecError _ -> True
  _             -> False

encodeGQResp :: GQResp -> EncJSON
encodeGQResp gqResp =
  encJFromAL $ case gqResp of
    GQSuccess r      -> [("data", r)]
    GQPreExecError e -> [("errors", encJFromJ e)]
    GQExecError e    -> [("data", "null"), ("errors", encJFromJ e)]
