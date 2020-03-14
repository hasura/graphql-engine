module Hasura.GraphQL.Schema where

import Hasura.Prelude

import qualified Data.HashMap.Strict.Extended as M
import qualified Language.GraphQL.Draft.Syntax as G

import qualified Hasura.RQL.DML.Select as RQL
import qualified Hasura.GraphQL.Parser as P

import Hasura.GraphQL.Parser (Parser, FieldsParser, Kind(..), UnpreparedValue(..))
import Hasura.GraphQL.Parser.Class
import Hasura.RQL.Types
import Hasura.SQL.Types
import Hasura.GraphQL.Schema.Common (qualifiedObjectToName)

type AnnotatedFields = RQL.AnnFldsG UnpreparedValue
type AnnotatedField = RQL.AnnFldG UnpreparedValue

-- | Corresponds to an object type for a table:
--
-- > type table {
-- >   col1: colty1
-- >   ...
-- >   rel1: relty1
-- > }
tableSelectionSet
  :: (MonadSchema n m, MonadError QErr m)
  => QualifiedTable
  -> m (Parser 'Output n AnnotatedFields)
tableSelectionSet = P.memoize 'tableSelectionSet \tableName -> do
  tableInfo <- _tiCoreInfo <$> askTableInfo tableName
  name <- qualifiedObjectToName $ _tciName tableInfo
  -- FIXME: permissions!
  fields <- catMaybes <$> traverse fieldSelection (M.elems $ _tciFieldInfoMap tableInfo)
  pure $ P.selectionSet name (_tciDescription tableInfo) $ catMaybes <$> sequenceA fields

-- | A field for a table. Returns 'Nothing' if the field’s name is not a valid
-- GraphQL 'Name'.
--
-- > field_name(arg_name: arg_type, ...): field_type
fieldSelection
  :: (MonadSchema n m, MonadError QErr m)
  => FieldInfo
  -> m (Maybe (FieldsParser 'Output n (Maybe (FieldName, AnnotatedField))))
fieldSelection fieldInfo = for (fieldInfoGraphQLName fieldInfo) \fieldName ->
  aliasToFieldName <$> case fieldInfo of
    FIColumn columnInfo -> do
      let annotated = RQL.mkAnnColField columnInfo Nothing -- FIXME: support ColOp
      field <- P.column (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)
      pure $ fmap (, annotated) <$> P.selection_ fieldName fieldDescription field
    FIRelationship relationshipInfo -> _ -- TODO: implement
    FIComputedField computedFieldInfo -> _ -- TODO: implement
  where
    aliasToFieldName = fmap $ fmap $ first $ FieldName . G.unName

    fieldDescription = case fieldInfo of
      FIColumn info -> pgiDescription info
      FIRelationship _ -> Nothing
      FIComputedField info -> _cffDescription $ _cfiFunction info
