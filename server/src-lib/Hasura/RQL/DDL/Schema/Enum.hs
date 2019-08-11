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

  -- * Loading enum values
  , fetchAndValidateEnumValues
  ) where

import           Hasura.Prelude

import           Control.Monad.Validate
import           Data.List                     (delete)

import qualified Data.HashMap.Strict           as M
import qualified Data.List.NonEmpty            as NE
import qualified Data.Text                     as T
import qualified Database.PG.Query             as Q
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.Db
import           Hasura.GraphQL.Utils
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Error
import           Hasura.SQL.Types

import qualified Hasura.SQL.DML                as S

data EnumTableIntegrityError
  = EnumTableMissingPrimaryKey
  | EnumTableMultiColumnPrimaryKey ![PGCol]
  | EnumTableNonTextualPrimaryKey !PGRawColumnInfo
  | EnumTableNoEnumValues
  | EnumTableInvalidEnumValueNames !(NE.NonEmpty T.Text)
  | EnumTableNonTextualCommentColumn !PGRawColumnInfo
  | EnumTableTooManyColumns ![PGCol]
  deriving (Show, Eq)

fetchAndValidateEnumValues
  :: (MonadTx m)
  => QualifiedTable
  -> [PGRawColumnInfo]
  -> [PGRawColumnInfo]
  -> m EnumValues
fetchAndValidateEnumValues tableName primaryKeyColumns columnInfos =
  either (throw400 ConstraintViolation . showErrors) pure =<< runValidateT fetchAndValidate
  where
    fetchAndValidate :: (MonadTx m, MonadValidate [EnumTableIntegrityError] m) => m EnumValues
    fetchAndValidate = do
      maybePrimaryKey <- tolerate validatePrimaryKey
      maybeCommentColumn <- validateColumns maybePrimaryKey
      enumValues <- maybe (refute mempty) (fetchEnumValues maybeCommentColumn) maybePrimaryKey
      validateEnumValues enumValues
      pure enumValues
      where
        validatePrimaryKey = case primaryKeyColumns of
          [] -> refute [EnumTableMissingPrimaryKey]
          [column] -> case prciType column of
            PGText -> pure column
            _      -> refute [EnumTableNonTextualPrimaryKey column]
          _ -> refute [EnumTableMultiColumnPrimaryKey $ map prciName primaryKeyColumns]

        validateColumns primaryKeyColumn = do
          let nonPrimaryKeyColumns = maybe columnInfos (`delete` columnInfos) primaryKeyColumn
          case nonPrimaryKeyColumns of
            [] -> pure Nothing
            [column] -> case prciType column of
              PGText -> pure $ Just column
              _ -> dispute [EnumTableNonTextualCommentColumn column] $> Nothing
            columns -> dispute [EnumTableTooManyColumns $ map prciName columns] $> Nothing

        fetchEnumValues maybeCommentColumn primaryKeyColumn = do
          let nullExtr = S.Extractor S.SENull Nothing
              commentExtr = maybe nullExtr (S.mkExtr . prciName) maybeCommentColumn
              query = Q.fromBuilder $ toSQL S.mkSelect
                { S.selFrom = Just $ S.mkSimpleFromExp tableName
                , S.selExtr = [S.mkExtr (prciName primaryKeyColumn), commentExtr] }
          fmap mkEnumValues . liftTx $ Q.withQE defaultTxErrorHandler query () True

        mkEnumValues rows = M.fromList . flip map rows $ \(key, comment) ->
          (EnumValue key, EnumValueInfo comment)

        validateEnumValues enumValues = do
          let enumValueNames = map (G.Name . getEnumValue) (M.keys enumValues)
          when (null enumValueNames) $
            refute [EnumTableNoEnumValues]
          let badNames = map G.unName $ filter (not . isValidEnumName) enumValueNames
          for_ (NE.nonEmpty badNames) $ \someBadNames ->
            refute [EnumTableInvalidEnumValueNames someBadNames]

        -- https://graphql.github.io/graphql-spec/June2018/#EnumValue
        isValidEnumName name =
          isValidName name && name `notElem` ["true", "false", "null"]

    showErrors :: [EnumTableIntegrityError] -> T.Text
    showErrors allErrors =
      "the table " <> tableName <<> " cannot be used as an enum " <> reasonsMessage
      where
        reasonsMessage = case allErrors of
          [singleError] -> "because " <> showOne singleError
          _ -> "for the following reasons:\n" <> T.unlines
            (map (("  • " <>) . showOne) allErrors)

        showOne :: EnumTableIntegrityError -> T.Text
        showOne = \case
          EnumTableMissingPrimaryKey -> "the table must have a primary key"
          EnumTableMultiColumnPrimaryKey cols ->
            "the table’s primary key must not span multiple columns ("
              <> T.intercalate ", " (map dquoteTxt $ sort cols) <> ")"
          EnumTableNonTextualPrimaryKey colInfo -> typeMismatch "primary key" colInfo PGText
          EnumTableNoEnumValues -> "the table must have at least one row"
          EnumTableInvalidEnumValueNames values ->
            let pluralString = " are not valid GraphQL enum value names"
                valuesString = case NE.reverse (NE.sort values) of
                  value NE.:| [] -> "value " <> value <<> " is not a valid GraphQL enum value name"
                  value2 NE.:| [value1] -> "values " <> value1 <<> " and " <> value2 <<> pluralString
                  lastValue NE.:| otherValues ->
                    "values " <> T.intercalate ", " (map dquoteTxt $ reverse otherValues) <> ", and "
                      <> lastValue <<> pluralString
            in "the " <> valuesString
          EnumTableNonTextualCommentColumn colInfo -> typeMismatch "comment column" colInfo PGText
          EnumTableTooManyColumns cols ->
            "the table must have exactly one primary key and optionally one comment column, not "
              <> T.pack (show $ length cols) <> " columns ("
              <> T.intercalate ", " (map dquoteTxt $ sort cols) <> ")"
          where
            typeMismatch description colInfo expected =
              "the table’s " <> description <> " (" <> prciName colInfo <<> ") must have type "
                <> expected <<> ", not type " <>> prciType colInfo
