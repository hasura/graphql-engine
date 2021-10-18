-- | Planning MySQL queries and subscriptions.
module Hasura.Backends.MySQL.Plan
  ( planQuery,
    queryToActionForest,
  )
where

import Control.Monad.Validate
import Data.Aeson qualified as J
import Data.ByteString.Lazy (toStrict)
import Data.Text.Extended
import Data.Tree
import Hasura.Backends.MySQL.DataLoader.Plan qualified as DataLoader
import Hasura.Backends.MySQL.FromIr
import Hasura.Backends.MySQL.Types
import Hasura.Base.Error
import Hasura.GraphQL.Parser qualified as GraphQL
import Hasura.Prelude hiding (first)
import Hasura.RQL.IR
import Hasura.RQL.Types.Column qualified as RQL
import Hasura.SQL.Backend
import Hasura.Session

-- | Plan the query and then produce a forest of actions for the executor.
queryToActionForest ::
  MonadError QErr m =>
  UserInfo ->
  QueryDB 'MySQL (Const Void) (GraphQL.UnpreparedValue 'MySQL) ->
  m (DataLoader.HeadAndTail, Forest DataLoader.PlannedAction)
queryToActionForest userInfo qrf = do
  select <- planQuery (_uiSession userInfo) qrf
  let (!headAndTail, !plannedActionsList) =
        DataLoader.runPlan
          (DataLoader.planSelectHeadAndTail Nothing Nothing select)
      !actionsForest = DataLoader.actionsForest id plannedActionsList
  pure (headAndTail, actionsForest)

planQuery ::
  MonadError QErr m =>
  SessionVariables ->
  QueryDB 'MySQL (Const Void) (GraphQL.UnpreparedValue 'MySQL) ->
  m Select
planQuery sessionVariables queryDB = do
  rootField <- traverse (prepareValueQuery sessionVariables) queryDB
  sel <-
    runValidate (runFromIr (fromRootField rootField))
      `onLeft` (throw400 NotSupported . tshow)
  pure $
    sel
      { selectFor = NoFor
      }

-- | Prepare a value without any query planning; we just execute the
-- query with the values embedded.
prepareValueQuery ::
  MonadError QErr m =>
  SessionVariables ->
  GraphQL.UnpreparedValue 'MySQL ->
  m Expression
prepareValueQuery sessionVariables =
  \case
    GraphQL.UVLiteral x -> pure x
    GraphQL.UVSession -> pure $ ValueExpression $ BinaryValue $ toStrict $ J.encode sessionVariables
    GraphQL.UVParameter _ RQL.ColumnValue {..} -> pure $ ValueExpression cvValue
    GraphQL.UVSessionVar _typ sessionVariable -> do
      value <-
        getSessionVariableValue sessionVariable sessionVariables
          `onNothing` throw400 NotFound ("missing session variable: " <>> sessionVariable)
      pure $ ValueExpression $ TextValue value
