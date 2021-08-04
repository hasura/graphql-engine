-- | Planning MySQL queries and subscriptions.

module Hasura.Backends.MySQL.Plan (planQuery) where

import           Control.Monad.Validate
import qualified Data.Aeson                   as J
import           Data.ByteString.Lazy         (toStrict)
import           Data.Text.Extended
import           Hasura.Backends.MySQL.FromIr
import           Hasura.Backends.MySQL.Types
import           Hasura.Base.Error
import qualified Hasura.GraphQL.Parser        as GraphQL
import           Hasura.Prelude               hiding (first)
import           Hasura.RQL.IR
import qualified Hasura.RQL.Types.Column      as RQL
import           Hasura.SQL.Backend
import           Hasura.Session

planQuery
  :: MonadError QErr m
  => SessionVariables
  -> QueryDB 'MySQL (Const Void) (GraphQL.UnpreparedValue 'MySQL)
  -> m Select
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
prepareValueQuery
  :: MonadError QErr m
  => SessionVariables
  -> GraphQL.UnpreparedValue 'MySQL
  -> m Expression
prepareValueQuery sessionVariables =
  \case
    GraphQL.UVLiteral x -> pure x
    GraphQL.UVSession -> pure $ ValueExpression $ BinaryValue $ toStrict $ J.encode sessionVariables
    GraphQL.UVParameter _ RQL.ColumnValue{..} -> pure $ ValueExpression cvValue
    GraphQL.UVSessionVar _typ sessionVariable -> do
      value <- getSessionVariableValue sessionVariable sessionVariables
        `onNothing` throw400 NotFound ("missing session variable: " <>> sessionVariable)
      pure $ ValueExpression $ TextValue value
