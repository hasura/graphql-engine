-- | Types and functions for interacting with and manipulating SQL enums represented by
-- /single-column tables/, __not__ native Postgres enum types. Native enum types in Postgres are
-- difficult to change, so we discourage their use, but we might add support for native enum types
-- in the future.
module Hasura.RQL.DDL.Schema.Enum (
  -- * Re-exports from "Hasura.RQL.Types.Column"
    EnumReference(..)
  , EnumValues
  , EnumValueInfo(..)
  , EnumValue(..)

  -- * Loading table info
  , resolveEnumReferences
  , fetchAndValidateEnumValues
  , fetchEnumValuesFromDb
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                 as M
import qualified Data.List.NonEmpty                  as NE
import qualified Data.Sequence                       as Seq
import qualified Data.Sequence.NonEmpty              as NESeq
import qualified Database.PG.Query                   as Q
import qualified Language.GraphQL.Draft.Syntax       as G

import           Control.Monad.Trans.Control         (MonadBaseControl)
import           Control.Monad.Validate
import           Data.List                           (delete)
import           Data.Text.Extended

import qualified Hasura.Backends.Postgres.SQL.DML    as S (Extractor (..), SQLExp (SENull), mkExtr,
                                                           mkSelect, mkSimpleFromExp, selExtr,
                                                           selFrom)

import           Hasura.Backends.Postgres.Connection
import           Hasura.Backends.Postgres.SQL.Types  hiding (TableName)
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Error
import           Hasura.Server.Utils                 (makeReasonMessage)
import           Hasura.SQL.Backend
import           Hasura.SQL.Types


-- | Given a map of enum tables, computes all enum references implied by the given set of foreign
-- keys. A foreign key constitutes an enum reference iff the following conditions hold:
--
--   1. The key only includes a single column.
--   2. The referenced column is the table’s primary key.
--   3. The referenced table is, in fact, an enum table.
resolveEnumReferences
  :: forall b
   . Backend b
  => HashMap (TableName b) (PrimaryKey (Column b), EnumValues)
  -> HashSet (ForeignKey b)
  -> HashMap (Column b) (NonEmpty (EnumReference b))
resolveEnumReferences enumTables =
  M.fromListWith (<>) . map (fmap (:|[])) . mapMaybe resolveEnumReference . toList
  where
    resolveEnumReference :: ForeignKey b -> Maybe (Column b, EnumReference b)
    resolveEnumReference foreignKey = do
      [(localColumn, foreignColumn)] <- pure $ M.toList (_fkColumnMapping @b foreignKey)
      (primaryKey, enumValues) <- M.lookup (_fkForeignTable foreignKey) enumTables
      guard (_pkColumns primaryKey == foreignColumn NESeq.:<|| Seq.Empty)
      pure (localColumn, EnumReference (_fkForeignTable foreignKey) enumValues)

data EnumTableIntegrityError (b :: BackendType)
  = EnumTablePostgresError !Text
  | EnumTableMissingPrimaryKey
  | EnumTableMultiColumnPrimaryKey ![Column b]
  | EnumTableNonTextualPrimaryKey !(RawColumnInfo b)
  | EnumTableNoEnumValues
  | EnumTableInvalidEnumValueNames !(NE.NonEmpty Text)
  | EnumTableNonTextualCommentColumn !(RawColumnInfo b)
  | EnumTableTooManyColumns ![Column b]

fetchAndValidateEnumValues
  :: (MonadIO m, MonadBaseControl IO m)
  => SourceConfig 'Postgres
  -> TableName 'Postgres
  -> Maybe (PrimaryKey (RawColumnInfo 'Postgres))
  -> [RawColumnInfo 'Postgres]
  -> m (Either QErr EnumValues)
fetchAndValidateEnumValues pgSourceConfig tableName maybePrimaryKey columnInfos = runExceptT $
  either (throw400 ConstraintViolation . showErrors) pure =<< runValidateT fetchAndValidate
  where
    fetchAndValidate
      :: (MonadIO m, MonadBaseControl IO m, MonadValidate [EnumTableIntegrityError 'Postgres] m)
      => m EnumValues
    fetchAndValidate = do
      maybePrimaryKeyColumn <- tolerate validatePrimaryKey
      maybeCommentColumn <- validateColumns maybePrimaryKeyColumn
      case maybePrimaryKeyColumn of
        Nothing               -> refute mempty
        Just primaryKeyColumn -> do
          result <- runPgSourceReadTx pgSourceConfig $ runValidateT $
                    fetchEnumValuesFromDb tableName primaryKeyColumn maybeCommentColumn
          case result of
            Left e             -> (refute . pure . EnumTablePostgresError . qeError) e
            Right (Left vErrs) -> refute vErrs
            Right (Right r)    -> pure r
      where
        validatePrimaryKey = case maybePrimaryKey of
          Nothing -> refute [EnumTableMissingPrimaryKey]
          Just primaryKey -> case _pkColumns primaryKey of
            column NESeq.:<|| Seq.Empty -> case prciType column of
              PGText -> pure column
              _      -> refute [EnumTableNonTextualPrimaryKey column]
            columns -> refute [EnumTableMultiColumnPrimaryKey $ map prciName (toList columns)]

        validateColumns primaryKeyColumn = do
          let nonPrimaryKeyColumns = maybe columnInfos (`delete` columnInfos) primaryKeyColumn
          case nonPrimaryKeyColumns of
            [] -> pure Nothing
            [column] -> case prciType column of
              PGText -> pure $ Just column
              _      -> dispute [EnumTableNonTextualCommentColumn column] $> Nothing
            columns -> dispute [EnumTableTooManyColumns $ map prciName columns] $> Nothing

    showErrors :: [EnumTableIntegrityError 'Postgres] -> Text
    showErrors allErrors =
      "the table " <> tableName <<> " cannot be used as an enum " <> reasonsMessage
      where
        reasonsMessage = makeReasonMessage allErrors showOne

        showOne :: EnumTableIntegrityError 'Postgres -> Text
        showOne = \case
          EnumTablePostgresError err -> "postgres error: " <> err
          EnumTableMissingPrimaryKey -> "the table must have a primary key"
          EnumTableMultiColumnPrimaryKey cols ->
            "the table’s primary key must not span multiple columns ("
              <> commaSeparated (sort cols) <> ")"
          EnumTableNonTextualPrimaryKey colInfo -> typeMismatch "primary key" colInfo PGText
          EnumTableNoEnumValues -> "the table must have at least one row"
          EnumTableInvalidEnumValueNames values ->
            let pluralString = " are not valid GraphQL enum value names"
                valuesString = case NE.reverse (NE.sort values) of
                  value NE.:| [] -> "value " <> value <<> " is not a valid GraphQL enum value name"
                  value2 NE.:| [value1] -> "values " <> value1 <<> " and " <> value2 <<> pluralString
                  lastValue NE.:| otherValues ->
                    "values " <> commaSeparated (reverse otherValues) <> ", and "
                      <> lastValue <<> pluralString
            in "the " <> valuesString
          EnumTableNonTextualCommentColumn colInfo -> typeMismatch "comment column" colInfo PGText
          EnumTableTooManyColumns cols ->
            "the table must have exactly one primary key and optionally one comment column, not "
              <> tshow (length cols) <> " columns ("
              <> commaSeparated (sort cols) <> ")"
          where
            typeMismatch description colInfo expected =
              "the table’s " <> description <> " (" <> prciName colInfo <<> ") must have type "
                <> expected <<> ", not type " <>> prciType colInfo

fetchEnumValuesFromDb
  :: (MonadTx m, MonadValidate [EnumTableIntegrityError 'Postgres] m)
  => TableName 'Postgres
  -> RawColumnInfo 'Postgres
  -> Maybe (RawColumnInfo 'Postgres)
  -> m EnumValues
fetchEnumValuesFromDb tableName primaryKeyColumn maybeCommentColumn = do
  let nullExtr = S.Extractor S.SENull Nothing
      commentExtr = maybe nullExtr (S.mkExtr . prciName) maybeCommentColumn
      query = Q.fromBuilder $ toSQL S.mkSelect
        { S.selFrom = Just $ S.mkSimpleFromExp tableName
        , S.selExtr = [S.mkExtr (prciName primaryKeyColumn), commentExtr] }
  rawEnumValues <- liftTx $ Q.withQE defaultTxErrorHandler query () True
  when (null rawEnumValues) $ dispute [EnumTableNoEnumValues]
  let enumValues = flip map rawEnumValues $
        \(enumValueText, comment) ->
          case mkValidEnumValueName enumValueText of
            Nothing        -> Left enumValueText
            Just enumValue -> Right (EnumValue enumValue, EnumValueInfo comment)
      badNames = lefts enumValues
      validEnums = rights enumValues
  case NE.nonEmpty badNames of
    Just someBadNames -> refute [EnumTableInvalidEnumValueNames someBadNames]
    Nothing           -> pure $ M.fromList validEnums
  where
    -- https://graphql.github.io/graphql-spec/June2018/#EnumValue
    mkValidEnumValueName name =
      if name `elem` ["true", "false", "null"] then Nothing
      else G.mkName name
