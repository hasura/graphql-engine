-- | Validate native queries against postgres-like flavors.
module Hasura.Backends.Postgres.Instances.NativeQueries
  ( validateNativeQuery,
    nativeQueryToPreparedStatement,
  )
where

import Data.Aeson (toJSON)
import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Extended (commaSeparated, dquoteList, toTxt)
import Data.Tuple (swap)
import Database.PG.Query qualified as PG
import Database.PostgreSQL.LibPQ qualified as PQ
import Hasura.Backends.Postgres.Connection qualified as PG
import Hasura.Backends.Postgres.Connection.Connect (withPostgresDB)
import Hasura.Backends.Postgres.Instances.Types ()
import Hasura.Backends.Postgres.SQL.Types (PGScalarType (..), pgScalarTypeToText)
import Hasura.Base.Error
import Hasura.LogicalModel.Cache (LogicalModelInfo (..))
import Hasura.LogicalModel.Common (columnsFromFields)
import Hasura.NativeQuery.InterpolatedQuery (trimQueryEnd)
import Hasura.NativeQuery.Metadata
  ( ArgumentName,
    InterpolatedItem (..),
    InterpolatedQuery (..),
    NativeQueryMetadata (..),
  )
import Hasura.NativeQuery.Types (NullableScalarType (nstType))
import Hasura.NativeQuery.Validation (validateArgumentDeclaration)
import Hasura.Prelude
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common (SourceName)

-- | Prepare a native query query against a postgres-like database to validate it.
validateNativeQuery ::
  forall m pgKind sourceConfig.
  (MonadIO m, MonadError QErr m) =>
  InsOrdHashMap.InsOrdHashMap PGScalarType PQ.Oid ->
  Env.Environment ->
  SourceName ->
  PG.PostgresConnConfiguration ->
  sourceConfig ->
  LogicalModelInfo ('Postgres pgKind) ->
  NativeQueryMetadata ('Postgres pgKind) ->
  m (InterpolatedQuery ArgumentName)
validateNativeQuery pgTypeOidMapping env sourceName connConf _sourceConfig logicalModel nq = do
  validateArgumentDeclaration nq
  let nqmCode = trimQueryEnd (_nqmCode nq)
      model = nq {_nqmCode = nqmCode}
  (prepname, preparedQuery) <- nativeQueryToPreparedStatement logicalModel model
  description <- runCheck prepname (PG.fromText preparedQuery)
  let returnColumns = bimap toTxt nstType <$> InsOrdHashMap.toList (columnsFromFields $ _lmiFields logicalModel)

  for_ (toList returnColumns) (matchTypes description)
  pure nqmCode
  where
    -- Run stuff against the database.
    --
    -- We don't need to deallocate the prepared statement because 'withPostgresDB'
    -- opens a new connection, runs a statement, and then closes the connection.
    -- Since a prepared statement only lasts for the duration of the session, once
    -- the session closes, the prepared statement is deallocated as well.
    runCheck :: BS.ByteString -> PG.Query -> m (PG.PreparedDescription PQ.Oid)
    runCheck prepname stmt =
      liftEither
        =<< liftIO
          ( withPostgresDB
              env
              sourceName
              connConf
              ( do
                  -- prepare statement
                  PG.rawQE @_ @()
                    ( \e ->
                        (err400 ValidationFailed "Failed to validate query")
                          { qeInternal = Just $ ExtraInternal $ toJSON e
                          }
                    )
                    stmt
                    []
                    False
                  -- extract description
                  PG.describePreparedStatement
                    ( \e ->
                        (err400 ValidationFailed "Failed to validate query")
                          { qeInternal = Just $ ExtraInternal $ toJSON e
                          }
                    )
                    prepname
              )
          )

    -- Look for the type for a particular column in the prepared statement description
    --   and compare them.
    --   fail if not found, try to provide a good error message if you can.
    matchTypes :: PG.PreparedDescription PQ.Oid -> (Text, PGScalarType) -> m ()
    matchTypes description (name, expectedType) =
      case lookup (Just (Text.encodeUtf8 name)) (PG.pd_fname_ftype description) of
        Nothing ->
          throwError
            (err400 ValidationFailed "Failed to validate query")
              { qeInternal =
                  Just
                    $ ExtraInternal
                    $ toJSON @Text
                    $ "Column named '"
                    <> toTxt name
                    <> "' is not returned from the query."
              }
        Just actualOid
          | Just expectedOid <- InsOrdHashMap.lookup expectedType pgTypeOidMapping,
            expectedOid /= actualOid ->
              throwError
                (err400 ValidationFailed "Failed to validate query")
                  { qeInternal =
                      Just
                        $ ExtraInternal
                        $ toJSON @Text
                        $ Text.unwords
                        $ [ "Return column '" <> name <> "' has a type mismatch.",
                            "The expected type is '" <> toTxt expectedType <> "',"
                          ]
                        <> case Map.lookup actualOid (invertPgTypeOidMap pgTypeOidMapping) of
                          Just t ->
                            ["but the actual type is '" <> toTxt t <> "'."]
                          Nothing ->
                            [ "and has the " <> tshow expectedOid <> ",",
                              "but the actual type has the " <> tshow actualOid <> "."
                            ]
                  }
        Just {} -> pure ()

-- | Invert the type/oid mapping.
invertPgTypeOidMap :: InsOrdHashMap PGScalarType PQ.Oid -> Map PQ.Oid PGScalarType
invertPgTypeOidMap = Map.fromList . map swap . InsOrdHashMap.toList

---------------------------------------

-- | The environment and fresh-name generator used by 'renameIQ'.
data RenamingState = RenamingState
  { rsNextFree :: Int,
    rsBoundVars :: Map ArgumentName Int
  }

-- | 'Rename' an 'InterpolatedQuery' expression with 'ArgumentName' variables
-- into one which uses ordinal arguments instead of named arguments, suitable
-- for a prepared query.
renameIQ ::
  InterpolatedQuery ArgumentName ->
  ( InterpolatedQuery Int,
    Map Int ArgumentName
  )
renameIQ = runRenaming . fmap InterpolatedQuery . mapM renameII . getInterpolatedQuery
  where
    runRenaming :: forall a. State RenamingState a -> (a, Map Int ArgumentName)
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
      InterpolatedItem ArgumentName ->
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
    inverseMap :: (Ord b) => Map a b -> Map b a
    inverseMap = Map.fromList . map swap . Map.toList

-- | Pretty print an interpolated query with numbered parameters.
renderIQ :: InterpolatedQuery Int -> Text
renderIQ (InterpolatedQuery items) = foldMap printItem items
  where
    printItem :: InterpolatedItem Int -> Text
    printItem (IIText t) = t
    printItem (IIVariable i) = "$" <> tshow i

-----------------------------------------

-- | Convert a native query to a prepared statement to be validate.
--
-- Used by 'validateNativeQuery'. Exported for testing.
nativeQueryToPreparedStatement ::
  forall m pgKind.
  (MonadError QErr m) =>
  LogicalModelInfo ('Postgres pgKind) ->
  NativeQueryMetadata ('Postgres pgKind) ->
  m (BS.ByteString, Text)
nativeQueryToPreparedStatement logicalModel model = do
  let (preparedIQ, argumentMapping) = renameIQ $ _nqmCode model
      logimoCode :: Text
      logimoCode = renderIQ preparedIQ
      prepname = "_logimo_vali_"

      occurringArguments, declaredArguments, undeclaredArguments :: Set ArgumentName
      occurringArguments = Set.fromList (Map.elems argumentMapping)
      declaredArguments = Set.fromList $ HashMap.keys (_nqmArguments model)
      undeclaredArguments = occurringArguments `Set.difference` declaredArguments

      argumentTypes :: Map Int PGScalarType
      argumentTypes = nstType <$> Map.fromList (HashMap.toList $ _nqmArguments model) `Map.compose` argumentMapping

      argumentSignature
        | argumentTypes /= mempty = "(" <> commaSeparated (pgScalarTypeToText <$> Map.elems argumentTypes) <> ")"
        | otherwise = ""

      returnedColumnNames :: Text
      returnedColumnNames =
        dquoteList $ InsOrdHashMap.keys (columnsFromFields $ _lmiFields logicalModel)

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

  when (Set.empty /= undeclaredArguments)
    $ throwError
    $ err400 ValidationFailed
    $ "Undeclared arguments: "
    <> commaSeparated (map tshow $ Set.toList undeclaredArguments)

  pure (Text.encodeUtf8 prepname, preparedQuery)
