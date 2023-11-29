module Hasura.GraphQL.Transport.HTTP.Protocol
  ( GQLReq (..),
    GQLBatchedReqs (..),
    GQLReqUnparsed,
    GQLReqParsed,
    GQLReqOutgoing,
    renderGQLReqOutgoing,
    SingleOperation,
    getSingleOperation,
    toParsed,
    getOpNameFromParsedReq,
    GQLQueryText (..),
    GQLExecDoc (..),
    OperationName (..),
    VariableValues,
    encodeGQErr,
    encodeGQExecError,
    encodeGQResp,
    decodeGQResp,
    encodeHTTPResp,
    GQResult,
    GQExecError (..),
    GQResponse,
    isExecError,
    ReqsText,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as J
import Data.Aeson.Encoding qualified as J
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.Either (isLeft)
import Data.HashMap.Strict qualified as HashMap
import Data.Text.Extended (dquote)
import Hasura.Base.Error
import Hasura.Base.Instances ()
import Hasura.EncJSON
import Hasura.GraphQL.Execute.Inline qualified as EI
import Hasura.Prelude
import Language.GraphQL.Draft.Parser qualified as G
import Language.GraphQL.Draft.Printer qualified as G
import Language.GraphQL.Draft.Syntax qualified as G
import Language.Haskell.TH.Syntax (Lift)

-- TODO: why not just `G.ExecutableDocument G.Name`?
newtype GQLExecDoc = GQLExecDoc {unGQLExecDoc :: [G.ExecutableDefinition G.Name]}
  deriving (Ord, Show, Eq, Hashable, Lift)

instance J.FromJSON GQLExecDoc where
  parseJSON v = GQLExecDoc . G.getExecutableDefinitions <$> J.parseJSON v

instance J.ToJSON GQLExecDoc where
  toJSON = J.toJSON . G.ExecutableDocument . unGQLExecDoc

newtype OperationName = OperationName {_unOperationName :: G.Name}
  deriving (Ord, Show, Eq, Hashable, J.ToJSON, Lift)

instance J.FromJSON OperationName where
  parseJSON v = OperationName <$> J.parseJSON v

type VariableValues = HashMap.HashMap G.Name J.Value

-- | https://graphql.org/learn/serving-over-http/#post-request
--
-- See 'GQLReqParsed' for invariants.
data GQLReq a = GQLReq
  { _grOperationName :: !(Maybe OperationName),
    _grQuery :: !a,
    _grVariables :: !(Maybe VariableValues)
  }
  deriving (Show, Eq, Generic, Functor, Lift)

instance (J.FromJSON a) => J.FromJSON (GQLReq a) where
  parseJSON = J.genericParseJSON (J.aesonPrefix J.camelCase) {J.omitNothingFields = True}

instance (J.ToJSON a) => J.ToJSON (GQLReq a) where
  toJSON = J.genericToJSON (J.aesonPrefix J.camelCase) {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding (J.aesonPrefix J.camelCase) {J.omitNothingFields = True}

instance (Hashable a) => Hashable (GQLReq a)

-- | Batched queries are sent as a JSON array of
-- 'GQLReq' records. This newtype exists to support
-- the unusual JSON encoding.
--
-- See <https://github.com/hasura/graphql-engine/issues/1812>.
data GQLBatchedReqs a
  = GQLSingleRequest a
  | GQLBatchedReqs [a]
  deriving (Show, Eq, Generic, Functor)

instance (J.ToJSON a) => J.ToJSON (GQLBatchedReqs a) where
  toJSON (GQLSingleRequest q) = J.toJSON q
  toJSON (GQLBatchedReqs qs) = J.toJSON qs

instance (J.FromJSON a) => J.FromJSON (GQLBatchedReqs a) where
  parseJSON arr@J.Array {} = GQLBatchedReqs <$> J.parseJSON arr
  parseJSON other = GQLSingleRequest <$> J.parseJSON other

newtype GQLQueryText = GQLQueryText
  { _unGQLQueryText :: Text
  }
  deriving (Show, Eq, Ord, Hashable, IsString)
  deriving newtype (J.FromJSON, J.ToJSON)

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

type ReqsText = GQLBatchedReqs (GQLReq GQLQueryText)

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
type GQLReqOutgoing = GQLReq SingleOperation

-- | A single graphql operation to be executed, with fragment definitions
-- inlined. This is the simplified form of 'GQLExecDoc' or
-- 'G.ExecutableDocument':
type SingleOperation = G.TypedOperationDefinition G.NoFragments G.Name

renderGQLReqOutgoing :: GQLReqOutgoing -> GQLReqUnparsed
renderGQLReqOutgoing = fmap (GQLQueryText . G.renderExecutableDoc . toExecDoc . inlineFrags)
  where
    -- This is essentially a 'coerce' (TODO unsafeCoerce optimization possible)?
    inlineFrags ::
      G.TypedOperationDefinition G.NoFragments var ->
      G.TypedOperationDefinition G.FragmentSpread var
    inlineFrags opDef =
      opDef {G._todSelectionSet = G.fmapSelectionSetFragment G.inline $ G._todSelectionSet opDef}
    toExecDoc =
      G.ExecutableDocument . pure . G.ExecutableDefinitionOperation . G.OperationDefinitionTyped

-- | Obtain the actual single operation to be executed, from the possibly-
-- multi-operation document, validating per the spec and inlining any
-- fragment definitions (pre-defined parts of a graphql query) at fragment
-- spreads (locations where fragments are "spliced"). See:
--
--     https://spec.graphql.org/June2018/#sec-Executable-Definitions  and...
--     https://graphql.org/learn/serving-over-http/
{-# INLINEABLE getSingleOperation #-}
getSingleOperation ::
  (MonadError QErr m) =>
  GQLReqParsed ->
  m SingleOperation
getSingleOperation (GQLReq opNameM q _varValsM) = do
  let (selSets, opDefs, fragments) = G.partitionExDefs $ unGQLExecDoc q
  G.TypedOperationDefinition {..} <-
    case (opNameM, selSets, opDefs) of
      (Just opName, [], _) -> do
        let n = _unOperationName opName
            opDefM = find (\opDef -> G._todName opDef == Just n) opDefs
        onNothing opDefM
          $ throw400 ValidationFailed
          $ "no such operation found in the document: "
          <> dquote n
      (Just _, _, _) ->
        throw400 ValidationFailed
          $ "operationName cannot be used when "
          <> "an anonymous operation exists in the document"
      (Nothing, [selSet], []) ->
        return $ G.TypedOperationDefinition G.OperationTypeQuery Nothing [] [] selSet
      (Nothing, [], [opDef]) ->
        return opDef
      (Nothing, _, _) ->
        throw400 ValidationFailed
          $ "exactly one operation has to be present "
          <> "in the document when operationName is not specified"

  inlinedSelSet <- EI.inlineSelectionSet fragments _todSelectionSet
  pure $ G.TypedOperationDefinition {_todSelectionSet = inlinedSelSet, ..}

toParsed :: (MonadError QErr m) => GQLReqUnparsed -> m GQLReqParsed
toParsed req = case G.parseExecutableDoc gqlText of
  Left _ -> withPathK "query" $ throw400 ValidationFailed "not a valid graphql query"
  Right a -> return $ req {_grQuery = GQLExecDoc $ G.getExecutableDefinitions a}
  where
    gqlText = _unGQLQueryText $ _grQuery req

-- | Get operation name from parsed executable document if the field `operationName` is not explicitly
-- sent by the client in the body of the request
getOpNameFromParsedReq :: GQLReqParsed -> Maybe OperationName
getOpNameFromParsedReq reqParsed =
  case execDefs of
    [G.ExecutableDefinitionOperation (G.OperationDefinitionTyped (G.TypedOperationDefinition _ maybeName _ _ _))] ->
      let maybeOpNameFromRequestBody = _grOperationName reqParsed
          maybeOpNameFromFirstExecDef = OperationName <$> maybeName
       in maybeOpNameFromRequestBody <|> maybeOpNameFromFirstExecDef
    _ -> _grOperationName reqParsed
  where
    execDefs = unGQLExecDoc $ _grQuery reqParsed

encodeGQErr :: Bool -> QErr -> J.Encoding
encodeGQErr includeInternal qErr =
  J.pairs (J.pair "errors" $ J.list id [encodeGQLErr includeInternal qErr])

type GQResult a = Either GQExecError a

newtype GQExecError = GQExecError [J.Encoding]
  deriving (Show, Eq)

type GQResponse = GQResult BL.ByteString

isExecError :: GQResult a -> Bool
isExecError = isLeft

encodeGQExecError :: GQExecError -> J.Encoding
encodeGQExecError (GQExecError errs) = J.list id errs

encodeGQResp :: GQResponse -> EncJSON
encodeGQResp gqResp =
  encJFromAssocList $ case gqResp of
    Right r -> [("data", encJFromLbsWithoutSoh r)]
    Left e -> [("data", encJFromBuilder "null"), ("errors", encJFromJEncoding $ encodeGQExecError e)]

-- We don't want to force the `Maybe GQResponse` unless absolutely necessary
-- Decode EncJSON from Cache for HTTP endpoints
decodeGQResp :: EncJSON -> (Maybe GQResponse, EncJSON)
decodeGQResp encJson =
  let gqResp =
        case J.decode @J.Value (encJToLBS encJson) of
          Just (J.Object v) ->
            case KM.lookup "error" v of
              Just err -> Just (Right $ J.encode err)
              Nothing -> Right . J.encode <$> KM.lookup "data" v
          _ -> Nothing
   in (gqResp, encJson)

-- Encode for HTTP Response without `data` envelope
encodeHTTPResp :: GQResponse -> EncJSON
encodeHTTPResp = \case
  Right r -> encJFromLBS r
  Left e -> encJFromJEncoding $ encodeGQExecError e
