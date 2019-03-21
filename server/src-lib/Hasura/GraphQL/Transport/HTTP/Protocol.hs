module Hasura.GraphQL.Transport.HTTP.Protocol
  ( GraphQLRequest(..)
  , GraphQLQuery(..)
  , OperationName(..)
  , VariableValues
  , encodeGQErr
  , encodeGQResp
  , GQResp(..)
  , isExecError
  ) where

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.Types

import qualified Data.Aeson                    as J
import qualified Data.Aeson.Casing             as J
import qualified Data.Aeson.TH                 as J
import qualified Data.ByteString.Lazy          as BL
import qualified Data.HashMap.Strict           as Map
import qualified Language.GraphQL.Draft.Parser as G
import qualified Language.GraphQL.Draft.Syntax as G

newtype GraphQLQuery
  = GraphQLQuery { unGraphQLQuery :: [G.ExecutableDefinition] }
  deriving (Show, Eq, Hashable)

instance J.FromJSON GraphQLQuery where
  parseJSON = J.withText "GraphQLQuery" $ \t ->
    case G.parseExecutableDoc t of
      Left _  -> fail "parsing the graphql query failed"
      Right q -> return $ GraphQLQuery $ G.getExecutableDefinitions q

instance J.ToJSON GraphQLQuery where
  -- TODO, add pretty printer in graphql-parser
  toJSON _ = J.String "toJSON not implemented for GraphQLQuery"

newtype OperationName
  = OperationName { _unOperationName :: G.Name }
  deriving (Show, Eq, Hashable, J.ToJSON)

instance J.FromJSON OperationName where
  parseJSON v = OperationName . G.Name <$> J.parseJSON v

type VariableValues = Map.HashMap G.Variable J.Value

data GraphQLRequest
  = GraphQLRequest
  { _grOperationName :: !(Maybe OperationName)
  , _grQuery         :: !GraphQLQuery
  , _grVariables     :: !(Maybe VariableValues)
  } deriving (Show, Eq, Generic)

$(J.deriveJSON (J.aesonDrop 3 J.camelCase){J.omitNothingFields=True}
  ''GraphQLRequest
 )

instance Hashable GraphQLRequest

encodeGQErr :: Bool -> QErr -> J.Value
encodeGQErr includeInternal qErr =
  J.object [ "errors" J..= [encodeGQLErr includeInternal qErr]]

data GQResp
  = GQSuccess !BL.ByteString
  | GQPreExecError ![J.Value]
  | GQExecError ![J.Value]
  deriving (Show, Eq)

isExecError :: GQResp -> Bool
isExecError = \case
  GQExecError _ -> True
  _             -> False

encodeGQResp :: GQResp -> EncJSON
encodeGQResp gqResp =
  encJFromAssocList $ case gqResp of
    GQSuccess r      -> [("data", encJFromLBS r)]
    GQPreExecError e -> [("errors", encJFromJValue e)]
    GQExecError e    -> [("data", "null"), ("errors", encJFromJValue e)]
