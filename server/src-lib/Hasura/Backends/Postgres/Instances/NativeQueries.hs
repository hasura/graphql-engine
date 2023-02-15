-- | Validate native queries against postgres-like flavors.
module Hasura.Backends.Postgres.Instances.NativeQueries
  ( validateNativeQuery,
  )
where

import Data.Aeson (toJSON)
import Data.Environment qualified as Env
import Data.Text.Extended (toTxt)
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection qualified as PG
import Hasura.Backends.Postgres.Connection.Connect (withPostgresDB)
import Hasura.Base.Error
import Hasura.NativeQuery.Metadata
import Hasura.Prelude
import Hasura.SQL.Backend

-- | Prepare a native query against a postgres-like database to validate it.
validateNativeQuery :: (MonadIO m, MonadError QErr m) => Env.Environment -> PG.PostgresConnConfiguration -> NativeQueryInfo ('Postgres pgKind) -> m ()
validateNativeQuery env connConf nativeQuery = do
  let name = getNativeQueryName $ nqiRootFieldName nativeQuery
  let code :: Text
      code = fold $ flip evalState (1 :: Int) do
        for (getInterpolatedQuery $ nqiCode nativeQuery) \case
          IIText t -> pure t
          IIVariable _v -> do
            i <- get
            modify (+ 1)
            pure $ "$" <> tshow i
  result <-
    liftIO $
      withPostgresDB env connConf $
        PG.rawQE
          ( \e ->
              (err400 ValidationFailed "Failed to validate query")
                { qeInternal = Just $ ExtraInternal $ toJSON e
                }
          )
          (PG.fromText $ "PREPARE _naqi_vali_" <> toTxt name <> " AS " <> code)
          []
          False
  case result of
    -- running the query failed
    Left err ->
      throwError err
    -- running the query succeeded
    Right () ->
      pure ()
