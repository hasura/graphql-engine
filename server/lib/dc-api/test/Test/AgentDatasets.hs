{-# LANGUAGE ConstraintKinds #-}

module Test.AgentDatasets
  ( DatasetContext (..),
    DatasetCloneInfo (..),
    usesDataset,
    chinookTemplate,
    functionsTemplate,
    testingEdgeCasesTemplate,
    HasDatasetContext,
    getDatasetContext,
    createClone,
    deleteClone,
  )
where

import Control.Monad (forM_, unless)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Maybe (isJust)
import Data.Text qualified as Text
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import GHC.Stack (HasCallStack)
import Hasura.Backends.DataConnector.API qualified as API
import Servant.API (NamedRoutes)
import Servant.Client (HasClient (..), (//))
import Servant.Client.Generic (genericClient)
import Test.AgentClient (HasAgentClient, runAgentClientT)
import Test.AgentTestContext (AgentTestContext (..), HasAgentTestContext, getAgentTestContext)
import Test.Sandwich (ExampleT, HasBaseContext, HasLabel, Label (..), LabelValue, SpecFree, expectationFailure, getContext, introduce, type (:>))
import Prelude

chinookTemplate :: API.DatasetTemplateName
chinookTemplate = API.DatasetTemplateName "Chinook"

functionsTemplate :: API.DatasetTemplateName
functionsTemplate = API.DatasetTemplateName "Functions"

testingEdgeCasesTemplate :: API.DatasetTemplateName
testingEdgeCasesTemplate = API.DatasetTemplateName "TestingEdgeCases"

-------------------------------------------------------------------------------

data DatasetContext = DatasetContext
  { _dcTemplateName :: API.DatasetTemplateName,
    _dcClone :: Maybe DatasetCloneInfo
  }

data DatasetCloneInfo = DatasetCloneInfo
  { _dciCloneName :: API.DatasetCloneName,
    _dciAgentConfig :: API.Config
  }

usesDataset ::
  forall context m.
  ( HasAgentClient context,
    HasAgentTestContext context,
    HasBaseContext context,
    MonadIO m,
    MonadThrow m
  ) =>
  API.DatasetTemplateName ->
  SpecFree (LabelValue "dataset-context" DatasetContext :> context) m () ->
  SpecFree context m ()
usesDataset datasetTemplateName =
  introduce label datasetContextLabel cloneTemplate deleteTemplate
  where
    cloneTemplate :: ExampleT context m DatasetContext
    cloneTemplate = runAgentClientT (Just "create") $ do
      AgentTestContext {..} <- getAgentTestContext
      cloneInfo <-
        if supportsDatasets _atcCapabilitiesResponse
          then do
            Just <$> createClone genericClient datasetTemplateName
          else pure Nothing
      pure $ DatasetContext datasetTemplateName cloneInfo

    deleteTemplate :: DatasetContext -> ExampleT context m ()
    deleteTemplate DatasetContext {..} =
      runAgentClientT (Just "delete") $
        forM_ _dcClone $
          deleteClone genericClient

    label :: String
    label = Text.unpack $ "Clone " <> API._unDatasetTemplateName datasetTemplateName <> " template"

datasetContextLabel :: Label "dataset-context" DatasetContext
datasetContextLabel = Label

type HasDatasetContext context = HasLabel context "dataset-context" DatasetContext

getDatasetContext :: (HasCallStack, HasDatasetContext context, MonadReader context m) => m DatasetContext
getDatasetContext = getContext datasetContextLabel

supportsDatasets :: API.CapabilitiesResponse -> Bool
supportsDatasets = isJust . API._cDatasets . API._crCapabilities

-------------------------------------------------------------------------------

createClone :: (MonadIO m) => Client m (NamedRoutes API.Routes) -> API.DatasetTemplateName -> m DatasetCloneInfo
createClone client datasetTemplateName = do
  cloneName <- liftIO $ API.DatasetCloneName . Text.replace "-" "" . UUID.toText <$> UUID.nextRandom
  let request = API.DatasetCreateCloneRequest datasetTemplateName
  API.DatasetCreateCloneResponse {..} <- (client // API._datasets // API._createClone) cloneName request
  pure $ DatasetCloneInfo cloneName _dccrConfig

deleteClone :: (MonadThrow m) => Client m (NamedRoutes API.Routes) -> DatasetCloneInfo -> m ()
deleteClone client DatasetCloneInfo {..} = do
  response@API.DatasetDeleteCloneResponse {..} <- (client // API._datasets // API._deleteClone) _dciCloneName
  unless (response == API.datasetDeleteCloneSuccess) $
    expectationFailure $
      "Deleting dataset clone " <> show _dciCloneName <> " failed with message: " <> Text.unpack _ddcrMessage
