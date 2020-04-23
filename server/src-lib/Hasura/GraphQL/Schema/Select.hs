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
    let desc = G.Description $ "fetch data from the table: \"" <> getTableTxt (qName tableName) <> "\""
    for selectPerms $ \perms -> selectFromTable tableName (Just desc) perms stringifyNum
    -- TODO: add aggregation tables and primary key queries?
  let queryFieldsParser = fmap (Map.fromList . catMaybes) $ sequenceA $ catMaybes selectExpParsers
  pure $ P.selectionSet $$(G.litName "Query") Nothing queryFieldsParser

selectFromTable
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable
  -> Maybe G.Description
  -> SelPermInfo
  -> Bool
  -> m (FieldsParser 'Output n (Maybe (G.Name, SelectExp)))
selectFromTable table description selectPermissions stringifyNum = do
  name               <- qualifiedObjectToName table
  tableArgsParser    <- tableArgs table
  selectionSetParser <- tableSelectionSet table selectPermissions stringifyNum
  pure $ P.selection name description tableArgsParser selectionSetParser <&> fmap
    \(aliasName, tableArgs, tableFields) ->
      ( aliasName
      , RQL.AnnSelG
        { RQL._asnFields   = tableFields
        , RQL._asnFrom     = RQL.FromTable table
        , RQL._asnPerm     = tablePermissions selectPermissions
        , RQL._asnArgs     = tableArgs
        , RQL._asnStrfyNum = stringifyNum
        }
      )

tableSelectPermissions
  :: forall m n. (MonadSchema n m)
  => QualifiedTable
  -> m (Maybe SelPermInfo)
tableSelectPermissions table = do
  roleName  <- askRoleName
  tableInfo <- _tiRolePermInfoMap <$> askTableInfo table
  pure $ _permSel =<< Map.lookup roleName tableInfo

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
-- >   distinct_on: [table_select_column!]
-- >   limit: Int
-- >   offset: Int
-- >   order_by: [table_order_by!]
-- >   where: table_bool_exp
-- > }
tableArgs
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable
  -> m (FieldsParser 'Input n TableArgs)
tableArgs table = do
  boolExpParser <- boolExp table
  pure $ do
    limit  <- P.fieldOptional limitName  Nothing P.int
    offset <- P.fieldOptional offsetName Nothing P.int
    whereF <- P.fieldOptional whereName  Nothing boolExpParser
    pure $ RQL.TableArgs
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
               then fmap (, annotated) <$> P.selection_ fieldName (pgiDescription columnInfo) field
               else pure Nothing
      FIRelationship relationshipInfo -> do
        let otherTable = riRTable  relationshipInfo
            colMapping = riMapping relationshipInfo
            relName    = riName    relationshipInfo
        tableSelectPermissions otherTable >>= \case
          Nothing    -> pure $ pure Nothing
          Just perms -> do
            -- FIXME: support aggregation
            let desc = Just $ G.Description $ case riType relationshipInfo of
                  ObjRel -> "An object relationship"
                  ArrRel -> "An array relationship"
            otherTableParser <- selectFromTable otherTable desc perms stringifyNum
            pure $ otherTableParser <&> fmap \(psName, selectExp) ->
              ( psName
              , let annotatedRelationship = RQL.AnnRelG relName colMapping selectExp
                in case riType relationshipInfo of
                  ObjRel -> RQL.FObj annotatedRelationship
                  ArrRel -> RQL.FArr $ RQL.ASSimple annotatedRelationship
              )
      FIComputedField computedFieldInfo ->
        computedField computedFieldInfo selectPermissions stringifyNum
  where
    aliasToFieldName = fmap $ fmap $ first $ FieldName . G.unName



{- WIP notes on computed field

if the underlying function has no args         => no "args" argument
if the underlying function has at least one    => "args" object argument

if the function's return type is a JSON scalar => "path" string argument for JSON colop
if it's a set of table field                   => table selection arguments (where, limit, offset, order_by, distinct_on)

the code has a branch to deal with the lack of "args" object and treat everything as positional, but GraphiQL doesn't seem to allow it? Is it for some other cases where we parse function arguments?

-}


-- | Parses the "args" argument of a computed field.
--   All arguments to the underlying function are parsed as an "args"
--   object. Named arguments are expected in a field with the same
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

jsonPathArg :: MonadParse n => PGScalarType -> FieldsParser 'Input n (Maybe RQL.ColOp)
jsonPathArg functionReturnType
  | isJSONType functionReturnType =
      P.fieldOptional fieldName description P.string `P.bindFields` traverse toColExp
  | otherwise = pure Nothing
  where
    fieldName = $$(G.litName "path")
    description = Just "JSON select path"
    toColExp textValue = case parseJSONPath textValue of
      Left err     -> parseError $ T.pack $ "parse json path error: " ++ err
      Right jPaths -> return $ RQL.ColOp S.jsonbPathOp $ S.SEArray $ map elToColExp jPaths
    elToColExp (Key k)   = S.SELit k
    elToColExp (Index i) = S.SELit $ T.pack (show i)

computedField
  :: (MonadSchema n m, MonadError QErr m)
  => ComputedFieldInfo
  -> SelPermInfo
  -> Bool
  -> m (FieldsParser 'Output n (Maybe (G.Name, AnnotatedField)))
computedField ComputedFieldInfo{..} selectPermissions stringifyNum = do
  fieldName <- textToName $ computedFieldNameToText $ _cfiName
  functionArgsParser <- computedFieldFunctionArgs _cfiFunction
  case _cfiReturnType of
    CFRScalar scalarReturnType ->
      if Set.member _cfiName $ spiScalarComputedFields selectPermissions
      then do
        let fieldArgsParser = do
              args  <- functionArgsParser
              colOp <- jsonPathArg scalarReturnType
              pure $ RQL.FComputedField $ RQL.CFSScalar $ RQL.ComputedFieldScalarSel
                { RQL._cfssFunction  = _cffName _cfiFunction
                , RQL._cfssType      = scalarReturnType
                , RQL._cfssColumnOp  = colOp
                , RQL._cfssArguments = args
                }
        dummyParser <- P.column (PGColumnScalar scalarReturnType) (G.Nullability False)
        pure $ P.selection fieldName fieldDescription fieldArgsParser dummyParser
      else pure $ pure Nothing
    CFRSetofTable tableName -> tableSelectPermissions tableName >>= \case
      Nothing    -> pure $ pure Nothing
      Just perms -> do
        -- WIP note: this is very similar to selectFromTable
        -- I wonder if it would make sense to merge them
        -- I'm erring on the side of no for now, but to be reconsidered
        -- when we clean the code after reaching feature parity
        selectArgsParser   <- tableArgs tableName
        selectionSetParser <- tableSelectionSet tableName perms stringifyNum
        let fieldArgsParser = liftA2 (,) functionArgsParser selectArgsParser
        pure $ P.selection fieldName Nothing fieldArgsParser selectionSetParser
          <&> fmap \(aliasName, (functionArgs, tableArgs), tableFields) ->
            ( aliasName
            , RQL.FComputedField $ RQL.CFSTable RQL.JASMultipleRows $ RQL.AnnSelG
              { RQL._asnFields   = tableFields
              , RQL._asnFrom     = RQL.FromFunction (_cffName _cfiFunction) functionArgs Nothing
              , RQL._asnPerm     = tablePermissions perms
              , RQL._asnArgs     = tableArgs
              , RQL._asnStrfyNum = stringifyNum
              }
            )
  where
    defaultDescription = "A computed field, executes function " <>> _cffName _cfiFunction
    fieldDescription = Just $ G.Description $ case _cffDescription _cfiFunction of
      Nothing                   -> defaultDescription
      Just (G.Description desc) -> T.unlines [desc, "", "", defaultDescription]
      -- WIP note: the original code contained one "\n" (^ here) instead
      -- I kept that behaviour but made it explicit
      -- I feel it's an error and should be only one ""?
