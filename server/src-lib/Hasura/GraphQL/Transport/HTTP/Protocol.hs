module Hasura.GraphQL.Transport.HTTP.Protocol
  ( GQLReq(..)
  , GQLBatchedReqs(..)
  , GQLReqUnparsed
  , GQLReqParsed
  , GQLReqOutgoing
  , renderGQLReqOutgoing
  , toParsed
  , GQLQueryText(..)
  , GQLExecDoc(..)
  , OperationName(..)
  , VariableValues
  , encodeGQErr
  , encodeGQResp
  , decodeGQResp
  , encodeHTTPResp
  , GQResult
  , GQExecError(..)
  , GQResponse
  , isExecError
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                     as J
import qualified Data.Aeson.Casing              as J
import qualified Data.Aeson.TH                  as J
import qualified Data.ByteString.Lazy           as BL
import qualified Data.HashMap.Strict            as Map
import qualified Language.GraphQL.Draft.Parser  as G
import qualified Language.GraphQL.Draft.Printer as G
import qualified Language.GraphQL.Draft.Syntax  as G

import           Data.Either                    (isLeft)
import           Language.Haskell.TH.Syntax     (Lift)

import           Hasura.Base.Error
import           Hasura.Base.Instances          ()
import           Hasura.EncJSON


-- TODO: why not just `G.ExecutableDocument G.Name`?
newtype GQLExecDoc
  = GQLExecDoc { unGQLExecDoc :: [G.ExecutableDefinition G.Name] }
  deriving (Ord, Show, Eq, Hashable, Lift)

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

-- | https://graphql.org/learn/serving-over-http/#post-request
--
-- See 'GQLReqParsed' for invariants.
data GQLReq a
  = GQLReq
  { _grOperationName :: !(Maybe OperationName)
  , _grQuery         :: !a
  , _grVariables     :: !(Maybe VariableValues)
  } deriving (Show, Eq, Generic, Functor, Lift)

$(J.deriveJSON (J.aesonPrefix J.camelCase){J.omitNothingFields=True} ''GQLReq)

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
  } deriving (Show, Eq, Ord, J.FromJSON, J.ToJSON, Hashable, IsString)

-- | We've not yet parsed the graphql query string parameter of the POST.
type GQLReqUnparsed = GQLReq GQLQueryText

-- | Invariants:
--
--    - when '_grOperationName' is @Nothing@, '_grQuery' contains exactly one
--      'ExecutableDefinitionOperation' (and zero or more 'ExecutableDefinitionFragment')
--
--    - when '_grOperationName' is present, there is a corresponding
--      'ExecutableDefinitionOperation' in '_grQuery'
type GQLReqParsed = GQLReq GQLExecDoc

-- | A simplified form of 'GQLReqParsed' which is more ergonomic in particular
-- for APIs that act as graphql /clients/ (e.g. in remote relationship
-- execution). This is a "desugared" request in which fragments have been
-- inlined (see 'inlineSelectionSet'), and the operation ('_grOperationName')
-- to be executed is the only payload (in contrast to a 'G.ExecutableDocument'
-- with possibly many named operations).
--
-- '_grOperationName' is essentially ignored here, but should correspond with
-- '_todName' if present.
--
-- These could maybe benefit from an HKD refactoring.
type GQLReqOutgoing = GQLReq (G.TypedOperationDefinition G.NoFragments G.Name)

renderGQLReqOutgoing :: GQLReqOutgoing -> GQLReqUnparsed
renderGQLReqOutgoing = fmap (GQLQueryText . G.renderExecutableDoc . toExecDoc . inlineFrags)
  where
    -- This is essentially a 'coerce' (TODO unsafeCoerce optimization possible)?
    inlineFrags :: G.TypedOperationDefinition G.NoFragments var
                -> G.TypedOperationDefinition G.FragmentSpread var
    inlineFrags opDef =
      opDef { G._todSelectionSet = G.fmapSelectionSetFragment G.inline $ G._todSelectionSet opDef }
    toExecDoc =
      G.ExecutableDocument . pure . G.ExecutableDefinitionOperation . G.OperationDefinitionTyped

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

-- We don't want to force the `Maybe GQResponse` unless absolutely necessary
-- Decode EncJSON from Cache for HTTP endpoints
decodeGQResp :: EncJSON -> (Maybe GQResponse, EncJSON)
decodeGQResp encJson =
  let gqResp =
        case J.decode @J.Value (encJToLBS encJson) of
          Just (J.Object v) ->
            case Map.lookup "error" v of
              Just err -> Just (Right $ J.encode err)
              Nothing  -> Right . J.encode <$> Map.lookup "data" v
          _ -> Nothing
  in (gqResp, encJson)

-- Encode for HTTP Response without `data` envelope
encodeHTTPResp :: GQResponse -> EncJSON
encodeHTTPResp = \case
  Right r -> encJFromLBS r
  Left e  -> encJFromJValue e
