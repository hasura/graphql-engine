-- | Validate logical models against postgres-like flavors.
module Hasura.Backends.Postgres.Instances.LogicalModels
  ( validateLogicalModel,
    logicalModelToPreparedStatement,
  )
where

import Data.Aeson (toJSON)
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Extended (commaSeparated, toTxt)
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection qualified as PG
import Hasura.Backends.Postgres.Connection.Connect (withPostgresDB)
import Hasura.Backends.Postgres.Instances.Types ()
import Hasura.Backends.Postgres.SQL.Types (PGScalarType, pgScalarTypeToText)
import Hasura.Base.Error
import Hasura.CustomReturnType.Metadata (CustomReturnTypeMetadata (..))
import Hasura.LogicalModel.Metadata
  ( InterpolatedItem (..),
    InterpolatedQuery (..),
    LogicalModelArgumentName,
    LogicalModelMetadata (..),
  )
import Hasura.LogicalModel.Types (NullableScalarType (nstType), getLogicalModelName)
import Hasura.Prelude
import Hasura.SQL.Backend

-- | Prepare a logical model query against a postgres-like database to validate it.
validateLogicalModel ::
  forall m pgKind.
  (MonadIO m, MonadError QErr m) =>
  Env.Environment ->
  PG.PostgresConnConfiguration ->
  CustomReturnTypeMetadata ('Postgres pgKind) ->
  LogicalModelMetadata ('Postgres pgKind) ->
  m ()
validateLogicalModel env connConf customReturnType model = do
  preparedQuery <- logicalModelToPreparedStatement customReturnType model

  -- We don't need to deallocate the prepared statement because 'withPostgresDB'
  -- opens a new connection, runs a statement, and then closes the connection.
  -- Since a prepared statement only lasts for the duration of the session, once
  -- the session closes, the prepared statement is deallocated as well.
  runRaw (PG.fromText $ preparedQuery)
  where
    runRaw :: PG.Query -> m ()
    runRaw stmt =
      liftEither
        =<< liftIO
          ( withPostgresDB
              env
              connConf
              ( PG.rawQE
                  ( \e ->
                      (err400 ValidationFailed "Failed to validate query")
                        { qeInternal = Just $ ExtraInternal $ toJSON e
                        }
                  )
                  stmt
                  []
                  False
              )
          )

---------------------------------------

-- | The environment and fresh-name generator used by 'renameIQ'.
data RenamingState = RenamingState
  { rsNextFree :: Int,
    rsBoundVars :: Map LogicalModelArgumentName Int
  }

-- | 'Rename' an 'InterpolatedQuery' expression with 'LogicalModelArgumentName' variables
-- into one which uses ordinal arguments instead of named arguments, suitable
-- for a prepared query.
renameIQ ::
  InterpolatedQuery LogicalModelArgumentName ->
  ( InterpolatedQuery Int,
    Map Int LogicalModelArgumentName
  )
renameIQ = runRenaming . fmap InterpolatedQuery . mapM renameII . getInterpolatedQuery
  where
    runRenaming :: forall a. State RenamingState a -> (a, Map Int LogicalModelArgumentName)
    runRenaming action =
      let (res, st) = runState action (RenamingState 1 mempty)
       in (res, inverseMap $ rsBoundVars st)

    drawFree :: State RenamingState Int
    drawFree = do
      i <- gets rsNextFree
      modify (\s -> s {rsNextFree = i + 1})
      return i

    -- Rename a variable, assigning a fresh argument index when encounting new
    -- variables and reusing the previously assigned indices when encountering a
    -- previously treated variable accordingly.
    renameII ::
      InterpolatedItem LogicalModelArgumentName ->
      State RenamingState (InterpolatedItem Int)
    renameII = traverse \v -> do
      env <- gets rsBoundVars
      (Map.lookup v env)
        `onNothing` ( do
                        i <- drawFree
                        modify \s -> s {rsBoundVars = Map.insert v i (rsBoundVars s)}
                        return i
                    )

    -- When renaming from the named representation to the ordinal representation
    -- it is convenient for the variable renaming environment to be keyed by the
    -- names.
    -- When subsequently rendering the prepared statement definition however, it
    -- is more convenient to inspect the environment by index.
    -- Therefore we invert the map as part of renaming.
    inverseMap :: Ord b => Map a b -> Map b a
    inverseMap = Map.fromList . map swap . Map.toList
      where
        swap (a, b) = (b, a)

-- | Pretty print an interpolated query with numbered parameters.
renderIQ :: InterpolatedQuery Int -> Text
renderIQ (InterpolatedQuery items) = foldMap printItem items
  where
    printItem :: InterpolatedItem Int -> Text
    printItem (IIText t) = t
    printItem (IIVariable i) = "$" <> tshow i

-----------------------------------------

-- | Convert a logical model to a prepared statement to be validate.
--
-- Used by 'validateLogicalModel'. Exported for testing.
logicalModelToPreparedStatement ::
  forall m pgKind.
  MonadError QErr m =>
  CustomReturnTypeMetadata ('Postgres pgKind) ->
  LogicalModelMetadata ('Postgres pgKind) ->
  m Text
logicalModelToPreparedStatement customReturnType model = do
  let name = getLogicalModelName $ _lmmRootFieldName model
  let (preparedIQ, argumentMapping) = renameIQ $ _lmmCode model
      logimoCode :: Text
      logimoCode = renderIQ preparedIQ
      prepname = "_logimo_vali_" <> toTxt name

      occurringArguments, declaredArguments, undeclaredArguments :: Set LogicalModelArgumentName
      occurringArguments = Set.fromList (Map.elems argumentMapping)
      declaredArguments = Set.fromList $ HashMap.keys (_lmmArguments model)
      undeclaredArguments = occurringArguments `Set.difference` declaredArguments

      argumentTypes :: Map Int PGScalarType
      argumentTypes = nstType <$> Map.fromList (HashMap.toList $ _lmmArguments model) `Map.compose` argumentMapping

      argumentSignature
        | argumentTypes /= mempty = "(" <> commaSeparated (pgScalarTypeToText <$> Map.elems argumentTypes) <> ")"
        | otherwise = ""

      returnedColumnNames :: Text
      returnedColumnNames =
        commaSeparated $ InsOrd.keys (_ctmFields customReturnType)

      wrapInCTE :: Text -> Text
      wrapInCTE query =
        Text.intercalate
          "\n"
          [ "WITH " <> ctename <> " AS (",
            query,
            ")",
            "SELECT " <> returnedColumnNames,
            "FROM " <> ctename
          ]
        where
          ctename = "_cte" <> prepname

      preparedQuery = "PREPARE " <> prepname <> argumentSignature <> " AS " <> wrapInCTE logimoCode

  when (Set.empty /= undeclaredArguments) $
    throwError $
      err400 ValidationFailed $
        "Undeclared arguments: " <> commaSeparated (map tshow $ Set.toList undeclaredArguments)

  return preparedQuery
