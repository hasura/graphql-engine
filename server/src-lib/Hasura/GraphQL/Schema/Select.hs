{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Hasura.GraphQL.Schema.Select where

import           Hasura.Prelude

import           Data.Either                   (partitionEithers)
import           Data.Foldable                 (toList)
import           Data.Maybe                    (fromJust)
import           Data.Parser.JSONPath
import           Data.Traversable              (mapAccumL)

import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as T
import qualified Language.GraphQL.Draft.Syntax as G

import qualified Hasura.GraphQL.Parser         as P
import qualified Hasura.RQL.DML.Select         as RQL
import qualified Hasura.SQL.DML                as S

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


-- | Corresponds to an object type for table arguments:
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


{- WIP notes on computed field

if the underlying function has no args         => no argument
if the underlying function has at least one    => "args" object argument
if the function's return type is a JSON scalar => "path" string argument for JSON colop
if it's a set of table field                   => table selection arguments (where, limit, offset, order_by, distinct_on)

the code has a branch to deal with the lack of "args" object and treat everything as positional, but GraphiQL doesn't seem to allow it? Is it for some other cases where we parse function arguments?

-}


-- | Parses the "args" argument of a computed field.
--   All arguments to the underlying function are parsed as an "args"
--   object. Named arguments are expecterd in a field with the same
--   name, while positional arguments are expected in an field named
--   "arg_$n".
--   Note that collisions are possible, but ignored for now.
--   (FIXME: link to an issue?)
computedFieldFunctionArgs
  :: (MonadSchema n m, MonadError QErr m)
  => ComputedFieldFunction
  -> m (FieldsParser 'Input n (RQL.FunctionArgsExpTableRow UnpreparedValue))
computedFieldFunctionArgs ComputedFieldFunction{..}
  | Seq.null _cffInputArgs = pure $ pure RQL.emptyFunctionArgsExp
  | otherwise = do
      argsParser <- sequenceA $ snd $ mapAccumL createField (1 :: Int) $ toList _cffInputArgs
      let argsDesc = G.Description $ "input parameters for function " <>> _cffName
          argsObj  = P.object objName Nothing $ sequenceA argsParser
          objName  = fromJust $ G.mkName $ getFunctionTxt (qName $ _cffName) <> "_args"
      pure $ P.field $$(G.litName "args") (Just argsDesc) argsObj <&> \args ->
        let (positional, Map.fromList -> named) = partitionEithers $ catMaybes args
            tableRowArg = RQL.AETableRow Nothing
        in case _cffTableArgument of
          FTAFirst -> RQL.FunctionArgsExp (tableRowArg:positional) named
          FTANamed argName index ->
            RQL.insertFunctionArg argName index tableRowArg $
              RQL.FunctionArgsExp positional named
  where
    -- This uses Either solely to use partitionEithers.
    -- Arbitrarily, Left is for positional fields, and Right for named fields.
    createField positionalIndex arg = case faName arg of
      Nothing   -> ( positionalIndex + 1
                   , fmap (fmap Left) <$> createField' arg ("arg_" <> T.pack (show positionalIndex))
                   )
      Just name -> ( positionalIndex
                   , let nameText = getFuncArgNameTxt name
                     in fmap (fmap $ Right . (nameText, )) <$> createField' arg nameText
                   )
    createField' arg name = do
      columnParser <- P.column (PGColumnScalar $ _qptName $ faType arg) (G.Nullability False)
      let fieldName  = fromJust $ G.mkName name
          -- ^ FIXME: there probably is a better way than using fromJust
          -- can we safely assume all field names are GraphQL-compatible? If not, why?
          argParser = if unHasDefault $ faHasDefault arg
                      then P.fieldOptional  fieldName Nothing columnParser
                      else Just <$> P.field fieldName Nothing columnParser
      pure $ fmap (RQL.AEInput . P.mkParameter) <$> argParser

computedFieldColOp
  :: (MonadSchema n m)
  => PGScalarType
  -> m (FieldsParser 'Input n (Maybe RQL.ColOp))
computedFieldColOp functionReturnType
  | isJSONType functionReturnType =
      pure $ P.fieldOptional $$(G.litName "path") (Just "JSON select path") P.string `P.bindFields` traverse toColExp
  | otherwise = pure $ pure Nothing
  where
    toColExp textValue = case parseJSONPath textValue of
      Left err     -> parseError $ T.pack $ "parse json path error: " ++ err
      Right jPaths -> return $ RQL.ColOp S.jsonbPathOp $ S.SEArray $ map elToColExp jPaths
    elToColExp (Key k)   = S.SELit k
    elToColExp (Index i) = S.SELit $ T.pack (show i)

computedField
  :: (MonadSchema n m, MonadError QErr m)
  => ComputedFieldInfo
  -> m (FieldsParser 'Output n (Maybe (G.Name, AnnotatedField)))
computedField ComputedFieldInfo{..} = case _cfiReturnType of
  CFRScalar scalarReturnType -> do
    case G.mkName $ computedFieldNameToText $ _cfiName of
      Nothing        -> pure $ pure Nothing
      Just fieldName -> do
        -- TODO: handle permissions
        dummyParser <- P.column (PGColumnScalar scalarReturnType) (G.Nullability False)
        argsParser  <- computedFieldFunctionArgs _cfiFunction
        jsonColOp   <- computedFieldColOp scalarReturnType
        let defaultDescription = "A computed field, executes function " <>> _cffName _cfiFunction
            fieldDescription = case _cffDescription _cfiFunction of
                Nothing                   -> defaultDescription
                Just (G.Description desc) -> T.unlines [desc, "", defaultDescription]
            arguments = liftA2 (,) argsParser jsonColOp
            parser = P.selection fieldName (Just $ G.Description fieldDescription) arguments dummyParser
            createResult (args, colOp) =
              RQL.FComputedField $ RQL.CFSScalar $ RQL.ComputedFieldScalarSel
                { RQL._cfssFunction  = _cffName _cfiFunction
                , RQL._cfssType      = scalarReturnType
                , RQL._cfssColumnOp  = colOp
                , RQL._cfssArguments = args
                }
        pure $ fmap (fmap createResult) <$> parser
  CFRSetofTable tableName -> undefined -- FIXME: todo
