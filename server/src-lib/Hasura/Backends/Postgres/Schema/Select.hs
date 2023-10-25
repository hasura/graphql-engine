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
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.Sequence qualified as Seq
import Data.Text.Casing qualified as C
import Data.Text.Extended
import Data.Traversable (mapAccumL)
import Hasura.Backends.Postgres.SQL.Types qualified as Postgres
import Hasura.Backends.Postgres.Types.ComputedField qualified as Postgres
import Hasura.Backends.Postgres.Types.Function qualified as Postgres
import Hasura.Base.Error
import Hasura.Function.Cache
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.BoolExp
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Parser
  ( FieldParser,
    InputFieldsParser,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Select
import Hasura.GraphQL.Schema.Table
import Hasura.GraphQL.Schema.Typename
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.NamingCase
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.RQL.Types.SchemaCache hiding (askTableInfo)
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.Server.Init.FeatureFlag qualified as FF
import Hasura.Table.Cache
import Language.GraphQL.Draft.Syntax qualified as G

-- | User-defined function (AKA custom function)
selectFunction ::
  forall r m n pgKind.
  ( MonadBuildSchema ('Postgres pgKind) r m n,
    BackendTableSelectSchema ('Postgres pgKind)
  ) =>
  MkRootFieldName ->
  -- | SQL function info
  FunctionInfo ('Postgres pgKind) ->
  -- | field description, if any
  Maybe G.Description ->
  SchemaT r m (Maybe (FieldParser n (SelectExp ('Postgres pgKind))))
selectFunction mkRootFieldName fi@FunctionInfo {..} description = runMaybeT do
  sourceInfo :: SourceInfo ('Postgres pgKind) <- asks getter
  roleName <- retrieve scRole
  let customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
  tableInfo <- lift $ askTableInfo _fiReturnType
  selectPermissions <- hoistMaybe $ tableSelectPermissions roleName tableInfo
  selectionSetParser <- MaybeT $ returnFunctionParser tableInfo
  lift do
    stringifyNumbers <- retrieve Options.soStringifyNumbers
    tableArgsParser <- tableArguments tableInfo
    functionArgsParser <- customSQLFunctionArgs fi _fiGQLName _fiGQLArgsName
    let argsParser = liftA2 (,) functionArgsParser tableArgsParser
        functionFieldName = runMkRootFieldName mkRootFieldName _fiGQLName
    pure
      $ P.subselection functionFieldName description argsParser selectionSetParser
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
  ( MonadBuildSchema ('Postgres pgKind) r m n,
    BackendTableSelectSchema ('Postgres pgKind)
  ) =>
  MkRootFieldName ->
  -- | SQL function info
  FunctionInfo ('Postgres pgKind) ->
  -- | field description, if any
  Maybe G.Description ->
  SchemaT r m (Maybe (FieldParser n (AggSelectExp ('Postgres pgKind))))
selectFunctionAggregate mkRootFieldName fi@FunctionInfo {..} description = runMaybeT do
  sourceInfo :: SourceInfo ('Postgres pgKind) <- asks getter
  roleName <- retrieve scRole
  let customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
      mkTypename = runMkTypename $ _rscTypeNames customization

  targetTableInfo <- askTableInfo _fiReturnType

  selectPermissions <- hoistMaybe $ tableSelectPermissions roleName targetTableInfo
  guard $ spiAllowAgg selectPermissions
  xNodesAgg <- hoistMaybe $ nodesAggExtension @('Postgres pgKind)
  nodesParser <- MaybeT $ tableSelectionList targetTableInfo
  lift do
    stringifyNumbers <- retrieve Options.soStringifyNumbers
    tableGQLName <- getTableIdentifierName targetTableInfo
    tableArgsParser <- tableArguments targetTableInfo
    functionArgsParser <- customSQLFunctionArgs fi _fiGQLAggregateName _fiGQLArgsName
    aggregateParser <- tableAggregationFields targetTableInfo
    let aggregateFieldName = runMkRootFieldName mkRootFieldName _fiGQLAggregateName
        argsParser = liftA2 (,) functionArgsParser tableArgsParser
        selectionName = mkTypename (applyTypeNameCaseIdentifier tCase $ mkTableAggregateTypeName tableGQLName)
        aggregationParser =
          fmap (parsedSelectionsToFields IR.TAFExp)
            $ P.nonNullableParser
            $ P.selectionSet
              selectionName
              Nothing
              [ IR.TAFNodes xNodesAgg <$> P.subselection_ Name._nodes Nothing nodesParser,
                IR.TAFAgg <$> P.subselection_ Name._aggregate Nothing aggregateParser
              ]
    pure
      $ P.subselection aggregateFieldName description argsParser aggregationParser
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
    AggregationPredicatesSchema ('Postgres pgKind),
    BackendTableSelectSchema ('Postgres pgKind)
  ) =>
  MkRootFieldName ->
  -- | SQL function info
  FunctionInfo ('Postgres pgKind) ->
  -- | field description, if any
  Maybe G.Description ->
  -- | primary key columns of the target table
  PrimaryKeyColumns ('Postgres pgKind) ->
  SchemaT r m (Maybe (FieldParser n (ConnectionSelectExp ('Postgres pgKind))))
selectFunctionConnection mkRootFieldName fi@FunctionInfo {..} description pkeyColumns = runMaybeT do
  sourceInfo :: SourceInfo ('Postgres pgKind) <- asks getter
  roleName <- retrieve scRole
  let customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization

  returnTableInfo <- lift $ askTableInfo _fiReturnType
  selectPermissions <- hoistMaybe $ tableSelectPermissions roleName returnTableInfo
  xRelayInfo <- hoistMaybe $ relayExtension @('Postgres pgKind)
  selectionSetParser <- MaybeT $ tableConnectionSelectionSet returnTableInfo
  lift do
    let fieldName = runMkRootFieldName mkRootFieldName $ _fiGQLName <> Name.__connection
    stringifyNumbers <- retrieve Options.soStringifyNumbers
    tableConnectionArgsParser <- tableConnectionArgs pkeyColumns returnTableInfo selectPermissions
    functionArgsParser <- customSQLFunctionArgs fi _fiGQLName _fiGQLArgsName
    let argsParser = liftA2 (,) functionArgsParser tableConnectionArgsParser
    pure
      $ P.subselection fieldName description argsParser selectionSetParser
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
  ( MonadBuildSchema ('Postgres pgKind) r m n,
    BackendTableSelectSchema ('Postgres pgKind)
  ) =>
  ComputedFieldInfo ('Postgres pgKind) ->
  TableName ('Postgres pgKind) ->
  TableInfo ('Postgres pgKind) ->
  SchemaT r m (Maybe (FieldParser n (AnnotatedField ('Postgres pgKind))))
computedFieldPG ComputedFieldInfo {..} parentTable tableInfo = runMaybeT do
  sourceInfo :: SourceInfo ('Postgres pgKind) <- asks getter
  roleName <- retrieve scRole
  let sourceName = _siName sourceInfo
      customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
  stringifyNumbers <- retrieve Options.soStringifyNumbers
  selectPermissions <- hoistMaybe $ tableSelectPermissions roleName tableInfo
  fieldName <- lift $ textToName $ computedFieldNameToText _cfiName
  functionArgsParser <- lift $ computedFieldFunctionArgs sourceName _cfiFunction
  case _cfiReturnType of
    Postgres.CFRScalar scalarReturnType -> do
      redactionExp <- hoistMaybe $ getRedactionExprForComputedField selectPermissions _cfiName
      let fieldArgsParser = do
            args <- functionArgsParser
            colOp <- scalarSelectionArgumentsParser @('Postgres pgKind) $ ColumnScalar scalarReturnType
            pure
              $ IR.AFComputedField
                _cfiXComputedFieldInfo
                _cfiName
                ( IR.CFSScalar
                    ( IR.ComputedFieldScalarSelect
                        { IR._cfssFunction = _cffName _cfiFunction,
                          IR._cfssType = scalarReturnType,
                          IR._cfssScalarArguments = colOp,
                          IR._cfssArguments = args,
                          IR._cfssRedactionExpression = redactionExp
                        }
                    )
                )
      dummyParser <- lift $ columnParser @('Postgres pgKind) (ColumnScalar scalarReturnType) (G.Nullability True)
      pure $ P.selection fieldName fieldDescription fieldArgsParser dummyParser
    Postgres.CFRSetofTable tableName -> do
      otherTableInfo <- lift $ askTableInfo tableName
      remotePerms <- hoistMaybe $ tableSelectPermissions roleName otherTableInfo
      selectionSetParser <- MaybeT (fmap (P.multiple . P.nonNullableParser) <$> tableSelectionSet otherTableInfo)
      selectArgsParser <- lift $ tableArguments otherTableInfo
      let fieldArgsParser = liftA2 (,) functionArgsParser selectArgsParser
      pure
        $ P.subselection fieldName fieldDescription fieldArgsParser selectionSetParser
        <&> \((functionArgs', args), fields) ->
          IR.AFComputedField _cfiXComputedFieldInfo _cfiName
            $ IR.CFSTable JASMultipleRows
            $ IR.AnnSelectG
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
      SourceName ->
      ComputedFieldFunction ('Postgres pgKind) ->
      SchemaT r m (InputFieldsParser n (FunctionArgsExp ('Postgres pgKind) (IR.UnpreparedValue ('Postgres pgKind))))
    computedFieldFunctionArgs sourceName ComputedFieldFunction {..} = do
      functionArgs (FTAComputedField _cfiName sourceName parentTable) (IAUserProvided <$> _cffInputArgs)
        <&> fmap addTableAndSessionArgument
      where
        addTableAndSessionArgument args@(FunctionArgsExp positional named) =
          let withTable = case Postgres._cffaTableArgument _cffComputedFieldImplicitArgs of
                Postgres.FTAFirst -> FunctionArgsExp (Postgres.AETableRow : positional) named
                Postgres.FTANamed argName index -> IR.insertFunctionArg argName index Postgres.AETableRow args
              sessionArgVal = Postgres.AESession IR.UVSession
           in case Postgres._cffaSessionArgument _cffComputedFieldImplicitArgs of
                Nothing -> withTable
                Just (Postgres.FunctionSessionArgument argName index) ->
                  IR.insertFunctionArg argName index sessionArgVal withTable

-- | The custom SQL functions' input "args" field parser
-- > function_name(args: function_args)
customSQLFunctionArgs ::
  (MonadBuildSchema ('Postgres pgKind) r m n) =>
  FunctionInfo ('Postgres pgKind) ->
  G.Name ->
  G.Name ->
  SchemaT r m (InputFieldsParser n (FunctionArgsExp ('Postgres pgKind) (IR.UnpreparedValue ('Postgres pgKind))))
customSQLFunctionArgs FunctionInfo {..} functionName functionArgsName =
  functionArgs
    ( FTACustomFunction
        $ CustomFunctionNames
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
  (MonadBuildSchema ('Postgres pgKind) r m n) =>
  FunctionTrackedAs ('Postgres pgKind) ->
  Seq.Seq (FunctionInputArgument ('Postgres pgKind)) ->
  SchemaT r m (InputFieldsParser n (FunctionArgsExp ('Postgres pgKind) (IR.UnpreparedValue ('Postgres pgKind))))
functionArgs functionTrackedAs (toList -> inputArgs) = do
  sourceInfo :: SourceInfo ('Postgres pgKind) <- asks getter
  enableNamingConventionSep2023 <- FF.checkFlag FF.namingConventionSep2023
  let customization = _siCustomization sourceInfo
      tCase
        | enableNamingConventionSep2023 = _rscNamingConvention customization
        | otherwise = HasuraCase
      mkTypename = runMkTypename $ _rscTypeNames customization
      -- First, we iterate through the original sql arguments in order, to find the
      -- corresponding graphql names. At the same time, we create the input field
      -- parsers, in three groups: session argument, optional arguments, and
      -- mandatory arguments. Optional arguments have a default value, mandatory
      -- arguments don't.
      (names, session, optional, mandatory) = mconcat $ snd $ mapAccumL (splitArguments tCase) 1 inputArgs
      defaultArguments = FunctionArgsExp (snd <$> session) HashMap.empty

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
            . applyTypeNameCaseIdentifier tCase
            <$> case functionTrackedAs of
              FTAComputedField computedFieldName _sourceName tableName -> do
                tableInfo <- askTableInfo tableName
                computedFieldGQLName <- textToName $ computedFieldNameToText computedFieldName
                tableGQLName <- getTableIdentifierName @('Postgres pgKind) tableInfo
                pure $ mkFunctionArgsTypeName computedFieldGQLName tableGQLName
              FTACustomFunction (CustomFunctionNames {cfnArgsName}) ->
                pure $ (if enableNamingConventionSep2023 then C.fromAutogeneratedName else C.fromCustomName) cfnArgsName
        let fieldName = Name._args
            fieldDesc =
              case functionTrackedAs of
                FTAComputedField computedFieldName _sourceName tableName ->
                  G.Description
                    $ "input parameters for computed field "
                    <> computedFieldName
                    <<> " defined on table "
                    <>> tableName
                FTACustomFunction (CustomFunctionNames {cfnFunctionName}) ->
                  G.Description $ "input parameters for function " <>> cfnFunctionName
            objectParser =
              P.object objectName Nothing (sequenceA argumentParsers) `P.bind` \arguments -> do
                -- After successfully parsing, we create a dictionary of the parsed fields
                -- and we re-iterate through the original list of sql arguments, now with
                -- the knowledge of their graphql name.
                let foundArguments = HashMap.fromList $ catMaybes arguments <> session
                    argsWithNames = zip names inputArgs

                -- All elements (in the orignal sql order) that are found in the result map
                -- are treated as positional arguments, whether they were originally named or
                -- not.
                (positional, left) <- spanMaybeM (\(name, _) -> pure $ HashMap.lookup name foundArguments) argsWithNames

                -- If there are arguments left, it means we found one that was not passed
                -- positionally. As a result, any remaining argument will have to be passed
                -- by name. We fail with a parse error if we encounter a positional sql
                -- argument (that does not have a name in the sql function), as:
                --   * only the last positional arguments can be omitted;
                --   * it has no name we can use.
                -- We also fail if we find a mandatory argument that was not
                -- provided by the user.
                named <- HashMap.fromList . catMaybes <$> traverse (namedArgument foundArguments) left
                pure $ FunctionArgsExp positional named
        pure
          $ if null mandatory
            then P.fieldOptional fieldName (Just fieldDesc) objectParser <&> fromMaybe emptyFunctionArgsExp
            else P.field fieldName (Just fieldDesc) objectParser
  where
    sessionPlaceholder :: Postgres.ArgumentExp (IR.UnpreparedValue b)
    sessionPlaceholder = Postgres.AEInput IR.UVSession

    splitArguments ::
      NamingCase ->
      Int ->
      FunctionInputArgument ('Postgres pgKind) ->
      ( Int,
        ( [Text], -- graphql names, in order
          [(Text, Postgres.ArgumentExp (IR.UnpreparedValue ('Postgres pgKind)))], -- session argument
          [SchemaT r m (InputFieldsParser n (Maybe (Text, Postgres.ArgumentExp (IR.UnpreparedValue ('Postgres pgKind)))))], -- optional argument
          [SchemaT r m (InputFieldsParser n (Maybe (Text, Postgres.ArgumentExp (IR.UnpreparedValue ('Postgres pgKind)))))] -- mandatory argument
        )
      )
    splitArguments _tCase positionalIndex (IASessionVariables name) =
      let argName = getFuncArgNameTxt name
       in ( positionalIndex,
            ( [argName],
              [(argName, sessionPlaceholder)],
              [],
              []
            )
          )
    splitArguments tCase positionalIndex (IAUserProvided arg) =
      let (argName, newIndex) = case Postgres.faName arg of
            Nothing -> ("arg_" <> tshow positionalIndex, positionalIndex + 1)
            Just name -> (getFuncArgNameTxt name, positionalIndex)
       in if Postgres.unHasDefault $ Postgres.faHasDefault arg
            then (newIndex, ([argName], [], [parseArgument tCase arg argName], []))
            else (newIndex, ([argName], [], [], [parseArgument tCase arg argName]))

    parseArgument :: NamingCase -> FunctionArgument ('Postgres pgKind) -> Text -> SchemaT r m (InputFieldsParser n (Maybe (Text, Postgres.ArgumentExp (IR.UnpreparedValue ('Postgres pgKind)))))
    parseArgument tCase arg name = do
      typedParser <- columnParser (ColumnScalar $ Postgres.mkFunctionArgScalarType $ Postgres.faType arg) (G.Nullability True)
      fieldName <- applyFieldNameCaseCust tCase <$> textToName name

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
      pure $ argParser `mapField` ((name,) . Postgres.AEInput . IR.mkParameter)

    namedArgument ::
      HashMap Text (Postgres.ArgumentExp (IR.UnpreparedValue ('Postgres pgKind))) ->
      (Text, FunctionInputArgument ('Postgres pgKind)) ->
      n (Maybe (Text, Postgres.ArgumentExp (IR.UnpreparedValue ('Postgres pgKind))))
    namedArgument dictionary (name, inputArgument) = case inputArgument of
      IASessionVariables _ -> pure $ Just (name, sessionPlaceholder)
      IAUserProvided arg -> case HashMap.lookup name dictionary of
        Just parsedValue -> case Postgres.faName arg of
          Just _ -> pure $ Just (name, parsedValue)
          Nothing -> P.parseErrorWith P.NotSupported "Only last set of positional arguments can be omitted"
        Nothing ->
          whenMaybe (not $ Postgres.unHasDefault $ Postgres.faHasDefault arg)
            $ P.parseErrorWith P.NotSupported "Non default arguments cannot be omitted"

buildFunctionQueryFieldsPG ::
  forall r m n pgKind.
  ( MonadBuildSchema ('Postgres pgKind) r m n,
    BackendTableSelectSchema ('Postgres pgKind)
  ) =>
  MkRootFieldName ->
  FunctionName ('Postgres pgKind) ->
  FunctionInfo ('Postgres pgKind) ->
  TableName ('Postgres pgKind) ->
  SchemaT r m [FieldParser n (QueryDB ('Postgres pgKind) (RemoteRelationshipField UnpreparedValue) (UnpreparedValue ('Postgres pgKind)))]
buildFunctionQueryFieldsPG mkRootFieldName functionName functionInfo tableName = do
  let -- select function
      funcDesc =
        Just
          . G.Description
          $ flip fromMaybe (_fiComment functionInfo)
          $ "execute function "
          <> functionName
          <<> " which returns "
          <>> tableName
      -- select function agg
      funcAggDesc =
        Just $ G.Description $ "execute function " <> functionName <<> " and query aggregates on result of table type " <>> tableName

      queryResultType =
        case _fiJsonAggSelect functionInfo of
          JASMultipleRows -> QDBMultipleRows
          JASSingleObject -> QDBSingleRow

  catMaybes
    <$> sequenceA
      [ optionalFieldParser (queryResultType) $ selectFunction mkRootFieldName functionInfo funcDesc,
        optionalFieldParser (QDBAggregation) $ selectFunctionAggregate mkRootFieldName functionInfo funcAggDesc
      ]

buildFunctionMutationFieldsPG ::
  forall r m n pgKind.
  ( MonadBuildSchema ('Postgres pgKind) r m n,
    BackendTableSelectSchema ('Postgres pgKind)
  ) =>
  MkRootFieldName ->
  FunctionName ('Postgres pgKind) ->
  FunctionInfo ('Postgres pgKind) ->
  TableName ('Postgres pgKind) ->
  SchemaT r m [FieldParser n (MutationDB ('Postgres pgKind) (RemoteRelationshipField UnpreparedValue) (UnpreparedValue ('Postgres pgKind)))]
buildFunctionMutationFieldsPG mkRootFieldName functionName functionInfo tableName = do
  let funcDesc = Just $ G.Description $ "execute VOLATILE function " <> functionName <<> " which returns " <>> tableName
      jsonAggSelect = _fiJsonAggSelect functionInfo
  catMaybes
    <$> sequenceA
      [ optionalFieldParser (MDBFunction jsonAggSelect) $ selectFunction mkRootFieldName functionInfo funcDesc
      -- TODO: do we want aggregate mutation functions?
      ]
