{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ViewPatterns #-}

module Hasura.Backends.Postgres.Schema.Select
  ( selectFunction,
    selectFunctionAggregate,
    selectFunctionConnection,
    computedFieldPG,
    buildFunctionQueryFieldsPG,
    buildFunctionMutationFieldsPG,
  )
where

import Control.Lens hiding (index)
import Data.Has (getter)
import Data.HashMap.Strict.Extended qualified as Map
import Data.Sequence qualified as Seq
import Data.Text.Extended
import Data.Traversable (mapAccumL)
import Hasura.Backends.Postgres.SQL.Types qualified as PG
import Hasura.Backends.Postgres.Types.ComputedField qualified as PG
import Hasura.Backends.Postgres.Types.Function qualified as PG
import Hasura.Base.Error
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.GraphQL.Schema.Parser
  ( FieldParser,
    InputFieldsParser,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Select
import Hasura.GraphQL.Schema.Table
import Hasura.GraphQL.Schema.Typename (mkTypename)
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Function
import Hasura.RQL.Types.SchemaCache hiding (askTableInfo)
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization (mkRootFieldName)
import Hasura.RQL.Types.Table
import Hasura.SQL.Backend
import Language.GraphQL.Draft.Syntax qualified as G

-- | User-defined function (AKA custom function)
selectFunction ::
  forall r m n pgKind.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  BackendTableSelectSchema ('Postgres pgKind) =>
  -- | source name
  SourceInfo ('Postgres pgKind) ->
  -- | SQL function info
  FunctionInfo ('Postgres pgKind) ->
  -- | field description, if any
  Maybe G.Description ->
  m (Maybe (FieldParser n (SelectExp ('Postgres pgKind))))
selectFunction sourceInfo fi@FunctionInfo {..} description = runMaybeT do
  tCase <- asks getter
  roleName <- retrieve scRole
  tableInfo <- lift $ askTableInfo sourceInfo _fiReturnType
  selectPermissions <- hoistMaybe $ tableSelectPermissions roleName tableInfo
  selectionSetParser <- MaybeT $ returnFunctionParser sourceInfo tableInfo
  lift do
    stringifyNumbers <- retrieve Options.soStringifyNumbers
    tableArgsParser <- tableArguments sourceInfo tableInfo
    functionArgsParser <- customSQLFunctionArgs sourceInfo fi _fiGQLName _fiGQLArgsName
    let argsParser = liftA2 (,) functionArgsParser tableArgsParser
    functionFieldName <- mkRootFieldName _fiGQLName
    pure $
      P.subselection functionFieldName description argsParser selectionSetParser
        <&> \((funcArgs, tableArgs'), fields) ->
          IR.AnnSelectG
            { IR._asnFields = fields,
              IR._asnFrom = IR.FromFunction _fiSQLName funcArgs Nothing,
              IR._asnPerm = tablePermissionsInfo selectPermissions,
              IR._asnArgs = tableArgs',
              IR._asnStrfyNum = stringifyNumbers,
              IR._asnNamingConvention = Just tCase
            }
  where
    returnFunctionParser =
      case _fiJsonAggSelect of
        JASSingleObject -> tableSelectionSet
        JASMultipleRows -> tableSelectionList

selectFunctionAggregate ::
  forall r m n pgKind.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  BackendTableSelectSchema ('Postgres pgKind) =>
  -- | source name
  SourceInfo ('Postgres pgKind) ->
  -- | SQL function info
  FunctionInfo ('Postgres pgKind) ->
  -- | field description, if any
  Maybe G.Description ->
  m (Maybe (FieldParser n (AggSelectExp ('Postgres pgKind))))
selectFunctionAggregate sourceInfo fi@FunctionInfo {..} description = runMaybeT do
  tCase <- asks getter
  roleName <- retrieve scRole
  targetTableInfo <- askTableInfo sourceInfo _fiReturnType
  selectPermissions <- hoistMaybe $ tableSelectPermissions roleName targetTableInfo
  guard $ spiAllowAgg selectPermissions
  xNodesAgg <- hoistMaybe $ nodesAggExtension @('Postgres pgKind)
  tableInfo <- askTableInfo sourceInfo _fiReturnType
  nodesParser <- MaybeT $ tableSelectionList sourceInfo tableInfo
  lift do
    stringifyNumbers <- retrieve Options.soStringifyNumbers
    tableGQLName <- getTableGQLName tableInfo
    tableArgsParser <- tableArguments sourceInfo tableInfo
    functionArgsParser <- customSQLFunctionArgs sourceInfo fi _fiGQLAggregateName _fiGQLArgsName
    aggregateParser <- tableAggregationFields sourceInfo tableInfo
    selectionName <- mkTypename =<< pure (tableGQLName <> Name.__aggregate)
    aggregateFieldName <- mkRootFieldName _fiGQLAggregateName
    let argsParser = liftA2 (,) functionArgsParser tableArgsParser
        aggregationParser =
          fmap (parsedSelectionsToFields IR.TAFExp) $
            P.nonNullableParser $
              P.selectionSet
                selectionName
                Nothing
                [ IR.TAFNodes xNodesAgg <$> P.subselection_ Name._nodes Nothing nodesParser,
                  IR.TAFAgg <$> P.subselection_ Name._aggregate Nothing aggregateParser
                ]
    pure $
      P.subselection aggregateFieldName description argsParser aggregationParser
        <&> \((funcArgs, tableArgs'), fields) ->
          IR.AnnSelectG
            { IR._asnFields = fields,
              IR._asnFrom = IR.FromFunction _fiSQLName funcArgs Nothing,
              IR._asnPerm = tablePermissionsInfo selectPermissions,
              IR._asnArgs = tableArgs',
              IR._asnStrfyNum = stringifyNumbers,
              IR._asnNamingConvention = Just tCase
            }

selectFunctionConnection ::
  forall pgKind r m n.
  ( MonadBuildSchema ('Postgres pgKind) r m n,
    BackendTableSelectSchema ('Postgres pgKind)
  ) =>
  -- | source name
  SourceInfo ('Postgres pgKind) ->
  -- | SQL function info
  FunctionInfo ('Postgres pgKind) ->
  -- | field description, if any
  Maybe G.Description ->
  -- | primary key columns of the target table
  PrimaryKeyColumns ('Postgres pgKind) ->
  m (Maybe (FieldParser n (ConnectionSelectExp ('Postgres pgKind))))
selectFunctionConnection sourceInfo fi@FunctionInfo {..} description pkeyColumns = runMaybeT do
  tCase <- asks getter
  roleName <- retrieve scRole
  returnTableInfo <- lift $ askTableInfo sourceInfo _fiReturnType
  selectPermissions <- hoistMaybe $ tableSelectPermissions roleName returnTableInfo
  xRelayInfo <- hoistMaybe $ relayExtension @('Postgres pgKind)
  tableInfo <- lift $ askTableInfo sourceInfo _fiReturnType
  selectionSetParser <- MaybeT $ tableConnectionSelectionSet sourceInfo tableInfo
  lift do
    fieldName <- mkRootFieldName $ _fiGQLName <> Name.__connection
    stringifyNumbers <- retrieve Options.soStringifyNumbers
    tableConnectionArgsParser <- tableConnectionArgs pkeyColumns sourceInfo tableInfo
    functionArgsParser <- customSQLFunctionArgs sourceInfo fi _fiGQLName _fiGQLArgsName
    let argsParser = liftA2 (,) functionArgsParser tableConnectionArgsParser
    pure $
      P.subselection fieldName description argsParser selectionSetParser
        <&> \((funcArgs, (args, split, slice)), fields) ->
          IR.ConnectionSelect
            { IR._csXRelay = xRelayInfo,
              IR._csPrimaryKeyColumns = pkeyColumns,
              IR._csSplit = split,
              IR._csSlice = slice,
              IR._csSelect =
                IR.AnnSelectG
                  { IR._asnFields = fields,
                    IR._asnFrom = IR.FromFunction _fiSQLName funcArgs Nothing,
                    IR._asnPerm = tablePermissionsInfo selectPermissions,
                    IR._asnArgs = args,
                    IR._asnStrfyNum = stringifyNumbers,
                    IR._asnNamingConvention = Just tCase
                  }
            }

-- | Computed field parser
computedFieldPG ::
  forall pgKind r m n.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  BackendTableSelectSchema ('Postgres pgKind) =>
  SourceInfo ('Postgres pgKind) ->
  ComputedFieldInfo ('Postgres pgKind) ->
  TableName ('Postgres pgKind) ->
  TableInfo ('Postgres pgKind) ->
  m (Maybe (FieldParser n (AnnotatedField ('Postgres pgKind))))
computedFieldPG sourceInfo ComputedFieldInfo {..} parentTable tableInfo = runMaybeT do
  tCase <- asks getter
  roleName <- retrieve scRole
  stringifyNumbers <- retrieve Options.soStringifyNumbers
  selectPermissions <- hoistMaybe $ tableSelectPermissions roleName tableInfo
  fieldName <- lift $ textToName $ computedFieldNameToText _cfiName
  functionArgsParser <- lift $ computedFieldFunctionArgs _cfiFunction
  case _cfiReturnType of
    PG.CFRScalar scalarReturnType -> do
      caseBoolExpMaybe <-
        hoistMaybe (Map.lookup _cfiName (spiComputedFields selectPermissions))
      let caseBoolExpUnpreparedValue =
            (fmap . fmap) partialSQLExpToUnpreparedValue <$> caseBoolExpMaybe
          fieldArgsParser = do
            args <- functionArgsParser
            colOp <- scalarSelectionArgumentsParser @('Postgres pgKind) $ ColumnScalar scalarReturnType
            pure $
              IR.AFComputedField
                _cfiXComputedFieldInfo
                _cfiName
                ( IR.CFSScalar
                    ( IR.ComputedFieldScalarSelect
                        { IR._cfssFunction = _cffName _cfiFunction,
                          IR._cfssType = scalarReturnType,
                          IR._cfssScalarArguments = colOp,
                          IR._cfssArguments = args
                        }
                    )
                    caseBoolExpUnpreparedValue
                )
      dummyParser <- lift $ columnParser @('Postgres pgKind) (ColumnScalar scalarReturnType) (G.Nullability True)
      pure $ P.selection fieldName fieldDescription fieldArgsParser dummyParser
    PG.CFRSetofTable tableName -> do
      otherTableInfo <- lift $ askTableInfo sourceInfo tableName
      remotePerms <- hoistMaybe $ tableSelectPermissions roleName otherTableInfo
      selectionSetParser <- MaybeT (fmap (P.multiple . P.nonNullableParser) <$> tableSelectionSet sourceInfo otherTableInfo)
      selectArgsParser <- lift $ tableArguments sourceInfo otherTableInfo
      let fieldArgsParser = liftA2 (,) functionArgsParser selectArgsParser
      pure $
        P.subselection fieldName fieldDescription fieldArgsParser selectionSetParser
          <&> \((functionArgs', args), fields) ->
            IR.AFComputedField _cfiXComputedFieldInfo _cfiName $
              IR.CFSTable JASMultipleRows $
                IR.AnnSelectG
                  { IR._asnFields = fields,
                    IR._asnFrom = IR.FromFunction (_cffName _cfiFunction) functionArgs' Nothing,
                    IR._asnPerm = tablePermissionsInfo remotePerms,
                    IR._asnArgs = args,
                    IR._asnStrfyNum = stringifyNumbers,
                    IR._asnNamingConvention = Just tCase
                  }
  where
    fieldDescription :: Maybe G.Description
    fieldDescription = G.Description <$> _cfiDescription

    computedFieldFunctionArgs ::
      ComputedFieldFunction ('Postgres pgKind) ->
      m (InputFieldsParser n (FunctionArgsExp ('Postgres pgKind) (IR.UnpreparedValue ('Postgres pgKind))))
    computedFieldFunctionArgs ComputedFieldFunction {..} =
      functionArgs sourceInfo (FTAComputedField _cfiName (_siName sourceInfo) parentTable) (IAUserProvided <$> _cffInputArgs)
        <&> fmap addTableAndSessionArgument
      where
        addTableAndSessionArgument args@(FunctionArgsExp positional named) =
          let withTable = case PG._cffaTableArgument _cffComputedFieldImplicitArgs of
                PG.FTAFirst -> FunctionArgsExp (PG.AETableRow : positional) named
                PG.FTANamed argName index -> IR.insertFunctionArg argName index PG.AETableRow args
              sessionArgVal = PG.AESession IR.UVSession
           in case PG._cffaSessionArgument _cffComputedFieldImplicitArgs of
                Nothing -> withTable
                Just (PG.FunctionSessionArgument argName index) ->
                  IR.insertFunctionArg argName index sessionArgVal withTable

-- | The custom SQL functions' input "args" field parser
-- > function_name(args: function_args)
customSQLFunctionArgs ::
  MonadBuildSchema ('Postgres pgKind) r m n =>
  SourceInfo ('Postgres pgKind) ->
  FunctionInfo ('Postgres pgKind) ->
  G.Name ->
  G.Name ->
  m (InputFieldsParser n (FunctionArgsExp ('Postgres pgKind) (IR.UnpreparedValue ('Postgres pgKind))))
customSQLFunctionArgs sourceInfo FunctionInfo {..} functionName functionArgsName =
  functionArgs
    sourceInfo
    ( FTACustomFunction $
        CustomFunctionNames
          { cfnFunctionName = functionName,
            cfnArgsName = functionArgsName
          }
    )
    _fiInputArgs

-- | Parses the arguments to the underlying sql function of a computed field or
--   a custom function. All arguments to the underlying sql function are parsed
--   as an "args" object. Named arguments are expected in a field with the same
--   name, while positional arguments are expected in an field named "arg_$n".
--   Note that collisions are possible, but ignored for now, if a named argument
--   is also named "arg_$n". (FIXME: link to an issue?)
--
--   If the function requires no argument, or if its only argument is not
--   user-provided (the session argument in the case of custom functions, the
--   table row argument in the case of computed fields), the args object will
--   be omitted.
functionArgs ::
  forall r m n pgKind.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  SourceInfo ('Postgres pgKind) ->
  FunctionTrackedAs ('Postgres pgKind) ->
  Seq.Seq (FunctionInputArgument ('Postgres pgKind)) ->
  m (InputFieldsParser n (FunctionArgsExp ('Postgres pgKind) (IR.UnpreparedValue ('Postgres pgKind))))
functionArgs sourceInfo functionTrackedAs (toList -> inputArgs) = do
  -- First, we iterate through the original sql arguments in order, to find the
  -- corresponding graphql names. At the same time, we create the input field
  -- parsers, in three groups: session argument, optional arguments, and
  -- mandatory arguments. Optional arguments have a default value, mandatory
  -- arguments don't.
  let (names, session, optional, mandatory) = mconcat $ snd $ mapAccumL splitArguments 1 inputArgs
      defaultArguments = FunctionArgsExp (snd <$> session) Map.empty

  if
      | length session > 1 ->
        -- We somehow found more than one session argument; this should never
        -- happen and is an error on our side.
        throw500 "there shouldn't be more than one session argument"
      | null optional && null mandatory ->
        -- There are no user-provided arguments to the function: there will be
        -- no args field.
        pure $ pure defaultArguments
      | otherwise -> do
        -- There are user-provided arguments: we need to parse an args object.
        argumentParsers <- sequenceA $ optional <> mandatory
        objectName <-
          mkTypename
            =<< case functionTrackedAs of
              FTAComputedField computedFieldName _sourceName tableName -> do
                tableInfo <- askTableInfo sourceInfo tableName
                computedFieldGQLName <- textToName $ computedFieldNameToText computedFieldName
                tableGQLName <- getTableGQLName @('Postgres pgKind) tableInfo
                pure $ computedFieldGQLName <> Name.__ <> tableGQLName <> Name.__args
              FTACustomFunction (CustomFunctionNames {cfnArgsName}) ->
                pure cfnArgsName
        let fieldName = Name._args
            fieldDesc =
              case functionTrackedAs of
                FTAComputedField computedFieldName _sourceName tableName ->
                  G.Description $
                    "input parameters for computed field "
                      <> computedFieldName <<> " defined on table " <>> tableName
                FTACustomFunction (CustomFunctionNames {cfnFunctionName}) ->
                  G.Description $ "input parameters for function " <>> cfnFunctionName
            objectParser =
              P.object objectName Nothing (sequenceA argumentParsers) `P.bind` \arguments -> do
                -- After successfully parsing, we create a dictionary of the parsed fields
                -- and we re-iterate through the original list of sql arguments, now with
                -- the knowledge of their graphql name.
                let foundArguments = Map.fromList $ catMaybes arguments <> session
                    argsWithNames = zip names inputArgs

                -- All elements (in the orignal sql order) that are found in the result map
                -- are treated as positional arguments, whether they were originally named or
                -- not.
                (positional, left) <- spanMaybeM (\(name, _) -> pure $ Map.lookup name foundArguments) argsWithNames

                -- If there are arguments left, it means we found one that was not passed
                -- positionally. As a result, any remaining argument will have to be passed
                -- by name. We fail with a parse error if we encounter a positional sql
                -- argument (that does not have a name in the sql function), as:
                --   * only the last positional arguments can be omitted;
                --   * it has no name we can use.
                -- We also fail if we find a mandatory argument that was not
                -- provided by the user.
                named <- Map.fromList . catMaybes <$> traverse (namedArgument foundArguments) left
                pure $ FunctionArgsExp positional named

        pure $ P.field fieldName (Just fieldDesc) objectParser
  where
    sessionPlaceholder :: PG.ArgumentExp (IR.UnpreparedValue b)
    sessionPlaceholder = PG.AEInput IR.UVSession

    splitArguments ::
      Int ->
      FunctionInputArgument ('Postgres pgKind) ->
      ( Int,
        ( [Text], -- graphql names, in order
          [(Text, PG.ArgumentExp (IR.UnpreparedValue ('Postgres pgKind)))], -- session argument
          [m (InputFieldsParser n (Maybe (Text, PG.ArgumentExp (IR.UnpreparedValue ('Postgres pgKind)))))], -- optional argument
          [m (InputFieldsParser n (Maybe (Text, PG.ArgumentExp (IR.UnpreparedValue ('Postgres pgKind)))))] -- mandatory argument
        )
      )
    splitArguments positionalIndex (IASessionVariables name) =
      let argName = getFuncArgNameTxt name
       in (positionalIndex, ([argName], [(argName, sessionPlaceholder)], [], []))
    splitArguments positionalIndex (IAUserProvided arg) =
      let (argName, newIndex) = case PG.faName arg of
            Nothing -> ("arg_" <> tshow positionalIndex, positionalIndex + 1)
            Just name -> (getFuncArgNameTxt name, positionalIndex)
       in if PG.unHasDefault $ PG.faHasDefault arg
            then (newIndex, ([argName], [], [parseArgument arg argName], []))
            else (newIndex, ([argName], [], [], [parseArgument arg argName]))

    parseArgument :: FunctionArgument ('Postgres pgKind) -> Text -> m (InputFieldsParser n (Maybe (Text, PG.ArgumentExp (IR.UnpreparedValue ('Postgres pgKind)))))
    parseArgument arg name = do
      typedParser <- columnParser (ColumnScalar $ PG.mkFunctionArgScalarType $ PG.faType arg) (G.Nullability True)
      fieldName <- textToName name

      -- Since all postgres function arguments are nullable, we define the
      -- GraphQL fields in nullable types. As explained in Note [When are fields
      -- optional?], this implies that they can be omitted. For backwards
      -- compatibility reasons, and also to avoid surprises, we prefer to reject
      -- the query if a mandatory argument is missing rather than filling the
      -- blanks for the user.
      --
      -- As explained in Note [The value of omitted fields], we can still reject
      -- queries when such nullable fields are omitted, and accept them when an
      -- explicit value of `null` is used, as long as we don't set a default
      -- value, not even `null`.
      let argParser = P.fieldOptional fieldName Nothing typedParser
      pure $ argParser `mapField` ((name,) . PG.AEInput . IR.mkParameter)

    namedArgument ::
      HashMap Text (PG.ArgumentExp (IR.UnpreparedValue ('Postgres pgKind))) ->
      (Text, FunctionInputArgument ('Postgres pgKind)) ->
      n (Maybe (Text, PG.ArgumentExp (IR.UnpreparedValue ('Postgres pgKind))))
    namedArgument dictionary (name, inputArgument) = case inputArgument of
      IASessionVariables _ -> pure $ Just (name, sessionPlaceholder)
      IAUserProvided arg -> case Map.lookup name dictionary of
        Just parsedValue -> case PG.faName arg of
          Just _ -> pure $ Just (name, parsedValue)
          Nothing -> P.parseErrorWith P.NotSupported "Only last set of positional arguments can be omitted"
        Nothing ->
          whenMaybe (not $ PG.unHasDefault $ PG.faHasDefault arg) $
            P.parseErrorWith P.NotSupported "Non default arguments cannot be omitted"

buildFunctionQueryFieldsPG ::
  forall r m n pgKind.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  BackendTableSelectSchema ('Postgres pgKind) =>
  SourceInfo ('Postgres pgKind) ->
  FunctionName ('Postgres pgKind) ->
  FunctionInfo ('Postgres pgKind) ->
  TableName ('Postgres pgKind) ->
  m [FieldParser n (QueryDB ('Postgres pgKind) (RemoteRelationshipField UnpreparedValue) (UnpreparedValue ('Postgres pgKind)))]
buildFunctionQueryFieldsPG sourceInfo functionName functionInfo tableName = do
  let -- select function
      funcDesc =
        Just . G.Description $
          flip fromMaybe (_fiComment functionInfo) $ "execute function " <> functionName <<> " which returns " <>> tableName
      -- select function agg

      funcAggDesc = Just $ G.Description $ "execute function " <> functionName <<> " and query aggregates on result of table type " <>> tableName

      queryResultType =
        case _fiJsonAggSelect functionInfo of
          JASMultipleRows -> QDBMultipleRows
          JASSingleObject -> QDBSingleRow

  catMaybes
    <$> sequenceA
      [ optionalFieldParser (queryResultType) $ selectFunction sourceInfo functionInfo funcDesc,
        optionalFieldParser (QDBAggregation) $ selectFunctionAggregate sourceInfo functionInfo funcAggDesc
      ]

buildFunctionMutationFieldsPG ::
  forall r m n pgKind.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  BackendTableSelectSchema ('Postgres pgKind) =>
  SourceInfo ('Postgres pgKind) ->
  FunctionName ('Postgres pgKind) ->
  FunctionInfo ('Postgres pgKind) ->
  TableName ('Postgres pgKind) ->
  m [FieldParser n (MutationDB ('Postgres pgKind) (RemoteRelationshipField UnpreparedValue) (UnpreparedValue ('Postgres pgKind)))]
buildFunctionMutationFieldsPG sourceInfo functionName functionInfo tableName = do
  let funcDesc = Just $ G.Description $ "execute VOLATILE function " <> functionName <<> " which returns " <>> tableName
      jsonAggSelect = _fiJsonAggSelect functionInfo
  catMaybes
    <$> sequenceA
      [ optionalFieldParser (MDBFunction jsonAggSelect) $ selectFunction sourceInfo functionInfo funcDesc
      -- TODO: do we want aggregate mutation functions?
      ]
