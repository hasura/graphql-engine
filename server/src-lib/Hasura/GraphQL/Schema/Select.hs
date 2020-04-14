module Hasura.GraphQL.Schema.Select where

import           Hasura.Prelude

import qualified Data.HashMap.Strict           as Map
import qualified Language.GraphQL.Draft.Syntax as G

import qualified Hasura.GraphQL.Parser         as P
import qualified Hasura.RQL.DML.Select         as RQL

import           Hasura.GraphQL.Parser         (FieldsParser, Kind (..), Parser,
                                                UnpreparedValue (..))
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Schema.BoolExp
import           Hasura.GraphQL.Schema.Common  (qualifiedObjectToName)
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value



type SelectExp  = RQL.AnnSimpleSelG UnpreparedValue
type TableArgs  = RQL.TableArgsG UnpreparedValue
type TablePerms = RQL.TablePermG UnpreparedValue


selectExp
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable
  -> TablePerms
  -> Bool
  -> m (FieldsParser 'Output n (Maybe (G.Name, SelectExp)))
selectExp table tablePermissions stringifyNum = do
  name               <- qualifiedObjectToName table
  tableArgsParser    <- tableArgs table
  selectionSetParser <- tableSelectionSet table
  return $ P.selection name Nothing tableArgsParser selectionSetParser <&> fmap
    \(aliasName, tableArgs, tableFields) -> (aliasName, RQL.AnnSelG
      { RQL._asnFields   = tableFields
      , RQL._asnFrom     = RQL.FromTable table
      , RQL._asnPerm     = tablePermissions
      , RQL._asnArgs     = tableArgs
      , RQL._asnStrfyNum = stringifyNum
      })


tablePerms
  :: forall m n. (MonadSchema n m)
  => QualifiedTable
  -> m (Maybe TablePerms)
tablePerms table = do
  roleName  <- askRoleName
  tableInfo <- _tiRolePermInfoMap <$> askTableInfo table
  return $ do
    rolePermissions   <- Map.lookup roleName tableInfo
    selectPermissions <- _permSel rolePermissions
    return $ RQL.TablePerm
      { RQL._tpFilter = fmapAnnBoolExp toUnpreparedValue $ spiFilter selectPermissions
      , RQL._tpLimit  = spiLimit selectPermissions
      }
  where
    toUnpreparedValue (PSESessVar pftype var) = P.UVSessionVar pftype var
    toUnpreparedValue (PSESQLExp exp)         = P.UVLiteral exp


tableArgs
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable
  -> m (FieldsParser 'Input n TableArgs)
tableArgs table = do
  boolExpParser <- boolExp table
  return $ do
    limit  <- P.fieldOptional limitName  Nothing P.int
    offset <- P.fieldOptional offsetName Nothing P.int
    whereF <- P.fieldOptional whereName  Nothing boolExpParser
    return $ RQL.TableArgs
      { RQL._taWhere    = whereF
      , RQL._taOrderBy  = Nothing -- TODO
      , RQL._taLimit    = fromIntegral <$> limit
      , RQL._taOffset   = txtEncoder . PGValInteger <$> offset
      , RQL._taDistCols = Nothing
      }
  where limitName  = $$(G.litName "limit")
        offsetName = $$(G.litName "offset")
        whereName  = $$(G.litName "where")

-- SELit . Text.pack . show <$>
{-
distinct_on: [card_types_select_column!]
limit: Intoffset: Int
order_by: [card_types_order_by!]
where: card_types_bool_exp
-}


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
  fields <- catMaybes <$> traverse fieldSelection (Map.elems $ _tciFieldInfoMap tableInfo)
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
    FIRelationship relationshipInfo -> undefined -- TODO: implement
    FIComputedField computedFieldInfo -> undefined -- TODO: implement
  where
    aliasToFieldName = fmap $ fmap $ first $ FieldName . G.unName

    fieldDescription = case fieldInfo of
      FIColumn info        -> pgiDescription info
      FIRelationship _     -> Nothing
      FIComputedField info -> _cffDescription $ _cfiFunction info
