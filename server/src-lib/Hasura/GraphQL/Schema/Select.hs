module Hasura.GraphQL.Schema.Select where

import           Hasura.Prelude

import           Data.Foldable                 (toList)
import           Data.Maybe                    (fromJust)

import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
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



type SelectExp       = RQL.AnnSimpleSelG UnpreparedValue
type TableArgs       = RQL.TableArgsG UnpreparedValue
type TablePerms      = RQL.TablePermG UnpreparedValue
type AnnotatedFields = RQL.AnnFldsG UnpreparedValue
type AnnotatedField  = RQL.AnnFldG UnpreparedValue


queryExp
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => HashSet QualifiedTable
  -> Bool
  -> m (Parser 'Output n (HashMap G.Name SelectExp))
queryExp allTables stringifyNum = do
  selectExpParsers <- for (toList allTables) $ \tableName -> do
    selectPerms <- tableSelectPermissions tableName
    for selectPerms $ \perms -> selectExp tableName perms stringifyNum
  let queryFieldsParser = fmap (Map.fromList . catMaybes) $ sequenceA $ catMaybes selectExpParsers
  pure $ P.selectionSet $$(G.litName "Query") Nothing queryFieldsParser

selectExp
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable
  -> SelPermInfo
  -> Bool
  -> m (FieldsParser 'Output n (Maybe (G.Name, SelectExp)))
selectExp table selectPermissions stringifyNum = do
  name               <- qualifiedObjectToName table
  tableArgsParser    <- tableArgs table
  selectionSetParser <- tableSelectionSet table selectPermissions stringifyNum
  return $ P.selection name Nothing tableArgsParser selectionSetParser <&> fmap
    \(aliasName, tableArgs, tableFields) -> (aliasName, RQL.AnnSelG
      { RQL._asnFields   = tableFields
      , RQL._asnFrom     = RQL.FromTable table
      , RQL._asnPerm     = tablePermissions selectPermissions
      , RQL._asnArgs     = tableArgs
      , RQL._asnStrfyNum = stringifyNum
      })

tableSelectPermissions
  :: forall m n. (MonadSchema n m)
  => QualifiedTable
  -> m (Maybe SelPermInfo)
tableSelectPermissions table = do
  roleName  <- askRoleName
  tableInfo <- _tiRolePermInfoMap <$> askTableInfo table
  return $ _permSel =<< Map.lookup roleName tableInfo

tablePermissions :: SelPermInfo -> TablePerms
tablePermissions selectPermissions =
  RQL.TablePerm { RQL._tpFilter = fmapAnnBoolExp toUnpreparedValue $ spiFilter selectPermissions
                , RQL._tpLimit  = spiLimit selectPermissions
                }
  where
    toUnpreparedValue (PSESessVar pftype var) = P.UVSessionVar pftype var
    toUnpreparedValue (PSESQLExp sqlExp)      = P.UVLiteral sqlExp


-- | Corresponds to an object type for table argumuments:
--
-- FIXME: is that the correct name?
-- > type table_arguments {
-- >   distinct_on: [card_types_select_column!]
-- >   limit: Int
-- >   offset: Int
-- >   order_by: [card_types_order_by!]
-- >   where: card_types_bool_exp
-- > }
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
      , RQL._taDistCols = Nothing -- TODO
      }
  where limitName  = $$(G.litName "limit")
        offsetName = $$(G.litName "offset")
        whereName  = $$(G.litName "where")


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
  -> SelPermInfo
  -> Bool
  -> m (Parser 'Output n AnnotatedFields)
tableSelectionSet tableName selectPermissions stringifyNum = memoizeOn 'tableSelectionSet tableName $ do
  tableInfo <- _tiCoreInfo <$> askTableInfo tableName
  name <- qualifiedObjectToName $ _tciName tableInfo
  fields <- fmap catMaybes
    $ traverse (\fieldInfo -> fieldSelection fieldInfo selectPermissions stringifyNum)
    $ Map.elems
    $ _tciFieldInfoMap tableInfo
  pure $ P.selectionSet name (_tciDescription tableInfo) $ catMaybes <$> sequenceA fields

-- | A field for a table. Returns 'Nothing' if the field’s name is not a valid
-- GraphQL 'Name'.
--
-- > field_name(arg_name: arg_type, ...): field_type
fieldSelection
  :: (MonadSchema n m, MonadError QErr m)
  => FieldInfo
  -> SelPermInfo
  -> Bool
  -> m (Maybe (FieldsParser 'Output n (Maybe (FieldName, AnnotatedField))))
fieldSelection fieldInfo selectPermissions stringifyNum =
  for (fieldInfoGraphQLName fieldInfo) \fieldName ->
    aliasToFieldName <$> case fieldInfo of
      FIColumn columnInfo -> do
        let annotated = RQL.mkAnnColField columnInfo Nothing -- FIXME: support ColOp
        field <- P.column (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)
        pure $ if Set.member (pgiColumn columnInfo) $ spiCols selectPermissions
               then fmap (, annotated) <$> P.selection_ fieldName fieldDescription field
               else pure Nothing
      FIRelationship relationshipInfo -> do
        let otherTable = riRTable  relationshipInfo
            colMapping = riMapping relationshipInfo
            relName    = riName    relationshipInfo
        tableSelectPermissions otherTable >>= \case
          Nothing    -> pure $ pure Nothing
          Just perms -> do
            otherTableParser <- selectExp otherTable perms stringifyNum
            pure $ otherTableParser <&> fmap \(psName, selectExp) ->
              ( psName
              , let annotatedRelationship = RQL.AnnRelG relName colMapping selectExp
                in case riType relationshipInfo of
                  -- FIXME: where do we set the description?
                  -- FIXME: support aggregation
                  ObjRel -> RQL.FObj annotatedRelationship
                  ArrRel -> RQL.FArr $ RQL.ASSimple annotatedRelationship
              )
      FIComputedField computedFieldInfo -> computedField computedFieldInfo
  where
    aliasToFieldName = fmap $ fmap $ first $ FieldName . G.unName

    fieldDescription = case fieldInfo of
      FIColumn info        -> pgiDescription info
      FIRelationship _     -> Nothing
      FIComputedField info -> _cffDescription $ _cfiFunction info


computedField
  :: (MonadSchema n m, MonadError QErr m)
  => ComputedFieldInfo
  -> m (FieldsParser 'Output n (Maybe (G.Name, AnnotatedField)))
computedField computedFieldInfo = case _cfiReturnType computedFieldInfo of
  CFRScalar scalarReturnType -> do
    case G.mkName $ computedFieldNameToText $ _cfiName computedFieldInfo of
      Nothing        -> pure $ pure Nothing
      Just fieldName -> do
        -- TODO: handle permissions
        fieldParser <- P.column (PGColumnScalar scalarReturnType) (G.Nullability True)
        argsParser  <- sequenceA <$>
          for (toList $ _cffInputArgs $ _cfiFunction computedFieldInfo) \arg ->
            case faName arg of
              Nothing   -> pure $ pure Nothing
              Just name -> do
                columnParser <- P.column
                  (PGColumnScalar $ _qptName $ faType arg)
                  (G.Nullability $ unHasDefault $ faHasDefault arg)
                let fieldName  = fromJust $ G.mkName $ getFuncArgNameTxt name
                    -- ^ FIXME: there probably is a better way than using fromJust
                    argParser = P.field fieldName Nothing columnParser
                pure $ argParser <&> \value ->
                  Just ( getFuncArgNameTxt name
                       , RQL.AEInput $ P.mkParameter value
                       )
        let funcName   = _cffName $ _cfiFunction $ computedFieldInfo
            objName    = fromJust $ G.mkName $ getFunctionTxt (qName $ funcName) <> "_args"
            -- ^ FIXME: there probably is a better way than using fromJust
            argsDesc   = G.Description $ "input parameters for function " <>> funcName
            argsObj    = P.object objName Nothing argsParser
            argsField  = P.field $$(G.litName "args") (Just argsDesc) argsObj
            fieldDesc  = _cffDescription $ _cfiFunction computedFieldInfo
            parser     = P.selection fieldName fieldDesc argsField fieldParser
        pure $ parser <&> \result -> do
          (name, args) <- result
          parsedArgs   <- sequenceA args
          pure ( name
               , RQL.FComputedField $ RQL.CFSScalar $ RQL.ComputedFieldScalarSel
                   { RQL._cfssFunction  = funcName
                   , RQL._cfssType      = scalarReturnType
                   , RQL._cfssColumnOp  = Nothing -- FIXME: support ColOp
                   , RQL._cfssArguments = RQL.FunctionArgsExp
                      { RQL._faePositional = []
                      , RQL._faeNamed      = Map.fromList parsedArgs
                      }
                   }
               )
  -- WIP: notes
  -- The code above is based on the following assumptions:
  --   - the _cffInputArgs are what the underlying SQL function expects,
  --     and should therefore be parsed as field of an args object
  --   - there is only one parameter to the computed field: an args object with
  --     said arguments
  -- Things I still find unclear:
  --   - what happens if we have Nothing for an arg name? How can it even happen?
  --   - what about positional arguments? old code seems to handle them...
  --   - is it okay to just "move the Maybes up" all the way back? Or do we want
  --     to explicitly fail here if we fail to parse a mandatory input?
  --   - can we safely assume all names are GraphQL-compatible? If not, why? What
  --     should we do on error?
  --   - is it okay to use the column parser for the field as a whole, as an argument
  --     to selection? I am not sure I understand how selection works...
  CFRSetofTable tableName -> undefined
