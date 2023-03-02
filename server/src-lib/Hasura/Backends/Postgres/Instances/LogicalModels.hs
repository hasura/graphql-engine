-- | Validate logical models against postgres-like flavors.
module Hasura.Backends.Postgres.Instances.LogicalModels
  ( validateLogicalModel,
  )
where

import Data.Aeson (toJSON)
import Data.Environment qualified as Env
import Data.Text.Extended (toTxt)
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection qualified as PG
import Hasura.Backends.Postgres.Connection.Connect (withPostgresDB)
import Hasura.Base.Error
import Hasura.LogicalModel.Metadata
import Hasura.Prelude
import Hasura.SQL.Backend

-- | Prepare a logical model query against a postgres-like database to validate it.
validateLogicalModel ::
  (MonadIO m, MonadError QErr m) =>
  Env.Environment ->
  PG.PostgresConnConfiguration ->
  LogicalModelInfo ('Postgres pgKind) ->
  m ()
validateLogicalModel env connConf model = do
  let name = getLogicalModelName $ lmiRootFieldName model
  let code :: Text
      code = fold $ flip evalState (1 :: Int) do
        for (getInterpolatedQuery $ lmiCode model) \case
          IIText t -> pure t
          IIVariable _v -> do
            i <- get
            modify (+ 1)
            pure $ "$" <> tshow i
      runRaw :: (MonadIO m, MonadError QErr m) => PG.Query -> m ()
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
      prepname = "_logimo_vali_" <> toTxt name

  -- We don't need to deallocate because 'withPostgresDB' opens a connection,
  -- runs a statement, and then closes the connection. Since a prepared statement only
  -- lasts the duration of the session, once it closes, it is deallocated as well.
  runRaw (PG.fromText $ "PREPARE " <> prepname <> " AS " <> code)
