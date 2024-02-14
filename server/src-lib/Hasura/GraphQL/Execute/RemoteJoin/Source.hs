-- | How to construct and execute a call to a source for a remote join.
--
-- There are three steps required to do this:
--   - construct the execution step for that source join
--   - execute that GraphQL query over the network
--   - build a join index of the variables out of the response
--
-- This can be done as one function, but we also export the individual steps for
-- debugging / test purposes. We congregate all intermediary state in the opaque
-- 'SourceJoinCall' type.
module Hasura.GraphQL.Execute.RemoteJoin.Source
  ( -- * Executing a remote join
    makeSourceJoinCall,

    -- * Individual steps
    SourceJoinCall (..),
    buildSourceJoinCall,
    buildJoinIndex,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Ordered qualified as AO
import Data.Aeson.Ordered qualified as JO
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty qualified as NE
import Data.Scientific qualified as Scientific
import Data.Text qualified as T
import Data.Text.Extended ((<<>), (<>>))
import Data.Text.Read qualified as TR
import Hasura.Base.Error
import Hasura.GraphQL.Execute.Backend qualified as EB
import Hasura.GraphQL.Execute.Instances ()
import Hasura.GraphQL.Execute.RemoteJoin.Types
import Hasura.GraphQL.Namespace
import Hasura.GraphQL.Transport.Instances ()
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.RQL.IR.ModelInformation (ModelInfoPart)
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Types
import Hasura.Session
import Hasura.Tracing (MonadTrace)
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Types qualified as HTTP

-------------------------------------------------------------------------------
-- Executing a remote join

-- | Construct and execute a call to a source for a remote join.
makeSourceJoinCall ::
  (MonadQueryTags m, MonadError QErr m, MonadTrace m, MonadIO m, MonadGetPolicies m) =>
  -- | Function to dispatch a request to a source.
  (AB.AnyBackend SourceJoinCall -> m BL.ByteString) ->
  -- | User information.
  UserInfo ->
  -- | Remote join information.
  AB.AnyBackend RemoteSourceJoin ->
  -- | Name of the field from the join arguments.
  FieldName ->
  -- | Mapping from 'JoinArgumentId' to its corresponding 'JoinArgument'.
  IntMap.IntMap JoinArgument ->
  [HTTP.Header] ->
  Maybe G.Name ->
  -- | The resulting join index (see 'buildJoinIndex') if any.
  m (Maybe (IntMap.IntMap AO.Value, [ModelInfoPart]))
makeSourceJoinCall networkFunction userInfo remoteSourceJoin jaFieldName joinArguments reqHeaders operationName =
  Tracing.newSpan ("Remote join to data source " <> sourceName <<> " for field " <>> jaFieldName) do
    -- step 1: create the SourceJoinCall
    -- maybeSourceCall <-
    --   AB.dispatchAnyBackend @EB.BackendExecute remoteSourceJoin \(sjc :: SourceJoinCall b) ->
    --     buildSourceJoinCall @b userInfo jaFieldName joinArguments sjc
    maybeSourceCall <-
      AB.dispatchAnyBackend @EB.BackendExecute remoteSourceJoin
        $ buildSourceJoinCall userInfo jaFieldName joinArguments reqHeaders operationName
    -- if there actually is a remote call:
    for maybeSourceCall \(sourceCall, modelInfoList) -> do
      -- step 2: send this call over the network
      sourceResponse <- networkFunction sourceCall
      -- step 3: build the join index
      Tracing.newSpan "Build remote join index"
        $ (,(modelInfoList))
        <$> buildJoinIndex sourceResponse
  where
    sourceName :: SourceName
    sourceName = AB.dispatchAnyBackend @Backend remoteSourceJoin _rsjSource

-------------------------------------------------------------------------------
-- Internal representation

-- | Intermediate type that contains all the necessary information to perform a
-- call to a database to perform a join.
data SourceJoinCall b = SourceJoinCall
  { _sjcRootFieldAlias :: RootFieldAlias,
    _sjcSourceConfig :: SourceConfig b,
    _sjcStepInfo :: EB.DBStepInfo b
  }

-------------------------------------------------------------------------------
-- Step 1: building the source call

buildSourceJoinCall ::
  forall b m.
  (EB.BackendExecute b, MonadQueryTags m, MonadError QErr m, MonadTrace m, MonadIO m, MonadGetPolicies m) =>
  UserInfo ->
  FieldName ->
  IntMap.IntMap JoinArgument ->
  [HTTP.Header] ->
  Maybe G.Name ->
  RemoteSourceJoin b ->
  m (Maybe (AB.AnyBackend SourceJoinCall, [ModelInfoPart]))
buildSourceJoinCall userInfo jaFieldName joinArguments reqHeaders operationName remoteSourceJoin = do
  Tracing.newSpan "Resolve execution step for remote join field" do
    let rows =
          IntMap.toList joinArguments <&> \(argumentId, argument) ->
            KM.insert "__argument_id__" (J.toJSON argumentId)
              $ KM.fromList
              $ map (bimap (K.fromText . getFieldNameTxt) JO.fromOrdered)
              $ HashMap.toList
              $ unJoinArgument argument
        rowSchema = fmap snd (_rsjJoinColumns remoteSourceJoin)
    for (NE.nonEmpty rows) $ \nonEmptyRows -> do
      let sourceConfig = _rsjSourceConfig remoteSourceJoin
      Tracing.attachSourceConfigAttributes @b sourceConfig
      (stepInfo, modelInfoList) <-
        EB.mkDBRemoteRelationshipPlan
          userInfo
          (_rsjSource remoteSourceJoin)
          sourceConfig
          nonEmptyRows
          rowSchema
          (FieldName "__argument_id__")
          (FieldName "f", _rsjRelationship remoteSourceJoin)
          reqHeaders
          operationName
          (_rsjStringifyNum remoteSourceJoin)
      -- This should never fail, as field names in remote relationships are
      -- validated when building the schema cache.
      fieldName <-
        G.mkName (getFieldNameTxt jaFieldName)
          `onNothing` throw500 ("'" <> getFieldNameTxt jaFieldName <> "' is not a valid GraphQL name")
      -- NOTE: We're making an assumption that the 'FieldName' propagated upwards
      -- from 'collectJoinArguments' is reasonable to use for logging.
      let rootFieldAlias = mkUnNamespacedRootFieldAlias fieldName
      pure
        $ ( AB.mkAnyBackend
              $ SourceJoinCall rootFieldAlias sourceConfig stepInfo,
            modelInfoList
          )

-------------------------------------------------------------------------------
-- Step 3: extracting the join index

-- | Construct a join index from the 'EncJSON' response from the source.
--
-- Unlike with remote schemas, we can make assumptions about the shape of the
-- result, instead of having to keep track of the path within the answer. This
-- function therefore enforces that the answer has the shape we expect, and
-- throws a 'QErr' if it doesn't.
buildJoinIndex :: (MonadError QErr m) => BL.ByteString -> m (IntMap.IntMap JO.Value)
buildJoinIndex response = do
  json <-
    JO.eitherDecode response `onLeft` \err ->
      throwInvalidJsonErr $ T.pack err
  case json of
    JO.Array arr -> fmap IntMap.fromList $ for (toList arr) \case
      JO.Object obj -> do
        argumentResult <-
          JO.lookup "f" obj
            `onNothing` throwMissingRelationshipDataErr
        argumentIdValue <-
          JO.lookup "__argument_id__" obj
            `onNothing` throwMissingArgumentIdErr
        argumentId <-
          case argumentIdValue of
            JO.Number n ->
              Scientific.toBoundedInteger n
                `onNothing` throwInvalidArgumentIdValueErr
            JO.String s ->
              intFromText s
                `onNothing` throwInvalidArgumentIdValueErr
            _ -> throwInvalidArgumentIdValueErr
        pure (argumentId, argumentResult)
      _ -> throwNoNestedObjectErr
    _ -> throwNoListOfObjectsErr
  where
    intFromText txt = case TR.decimal txt of
      Right (i, "") -> pure i
      _ -> Nothing
    throwInvalidJsonErr errMsg =
      throw500
        $ "failed to decode JSON response from the source: "
        <> errMsg
    throwMissingRelationshipDataErr =
      throw500
        $ "cannot find relationship data (aliased as 'f') within the source \
          \response"
    throwMissingArgumentIdErr =
      throw500
        $ "cannot find '__argument_id__' within the source response"
    throwInvalidArgumentIdValueErr =
      throw500 $ "expected 'argument_id' to get parsed as backend integer type"
    throwNoNestedObjectErr =
      throw500
        $ "expected an object one level deep in the remote schema's response, \
          \but found an array/scalar value instead"
    throwNoListOfObjectsErr =
      throw500
        $ "expected a list of objects in the remote schema's response, but found \
          \an object/scalar value instead"
