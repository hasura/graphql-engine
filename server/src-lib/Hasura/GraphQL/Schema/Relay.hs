{-# LANGUAGE TemplateHaskellQuotes #-}

-- | This module only exposes one function, 'nodeField', which is used at the
-- root level of the schema to create the 'node' field in the Relay API schema.
module Hasura.GraphQL.Schema.Relay
  ( nodeInterface,
    nodeField,
  )
where

import Control.Lens hiding (index)
import Data.Aeson qualified as J
import Data.Aeson.Types qualified as J
import Data.Align (align)
import Data.Has
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Data.These (partitionThese)
import Hasura.Base.Error
import Hasura.Base.ErrorMessage
import Hasura.Base.ToErrorValue
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Instances ()
import Hasura.GraphQL.Schema.Node
import Hasura.GraphQL.Schema.Parser (Kind (..), Parser, memoizeOn)
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Table
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.RQL.Types.SchemaCache hiding (askTableInfo)
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Table.Cache
import Language.GraphQL.Draft.Syntax qualified as G

-- | Constructs the parser for the node interface.
--
-- As mentioned in Note [Internal Relay HashMap], this function must parse an
-- incoming query for ANY potential matching table. Its resulting parser returns
-- a 'NodeMap': a container that, to a source name and a table name, associates
-- both the parsed fields and all the relevant table information required to
-- craft a request.
nodeInterface :: SourceCache -> NodeInterfaceParserBuilder
nodeInterface sourceCache = NodeInterfaceParserBuilder $ \context options -> memoizeOn 'nodeInterface () do
  let idDescription = G.Description "A globally unique identifier"
      idField = P.selection_ Name._id (Just idDescription) P.identifier
      nodeInterfaceDescription = G.Description "An object with globally unique ID"
      roleName = scRole context
  tables :: [Parser 'Output n (SourceName, AB.AnyBackend TableMap)] <-
    catMaybes . concat <$> for (HashMap.toList sourceCache) \(sourceName, anySourceInfo) ->
      AB.dispatchAnyBackendWithTwoConstraints @BackendSchema @BackendTableSelectSchema
        anySourceInfo
        \(sourceInfo :: SourceInfo b) ->
          runSourceSchema context options sourceInfo do
            for (HashMap.toList $ takeValidTables $ _siTables sourceInfo) \(tableName, tableInfo) -> runMaybeT do
              tablePkeyColumns <- hoistMaybe $ tableInfo ^? tiCoreInfo . tciPrimaryKey . _Just . pkColumns
              selectPermissions <- hoistMaybe $ tableSelectPermissions roleName tableInfo
              annotatedFieldsParser <- MaybeT $ tableSelectionSet tableInfo
              pure
                $ annotatedFieldsParser
                <&> \fields ->
                  ( sourceName,
                    AB.mkAnyBackend
                      $ TableMap
                      $ HashMap.singleton tableName
                      $ NodeInfo sourceInfo selectPermissions tablePkeyColumns fields
                  )
  pure
    $ HashMap.fromListWith fuseAnyMaps
    <$> P.selectionSetInterface
      Name._Node
      (Just nodeInterfaceDescription)
      [idField]
      tables
  where
    -- this can only ever fail if somehow, within the same source, we ran into
    -- two tables of a different type b; in other words, it is impossible.
    fuseAnyMaps :: AB.AnyBackend TableMap -> AB.AnyBackend TableMap -> AB.AnyBackend TableMap
    fuseAnyMaps m1 m2 =
      AB.composeAnyBackend @Backend fuseMaps m1 m2
        $ error "panic: two tables of a different backend type within the same source"

    fuseMaps :: forall b. (Backend b) => TableMap b -> TableMap b -> AB.AnyBackend TableMap
    fuseMaps (TableMap m1) (TableMap m2) = AB.mkAnyBackend @b $ TableMap $ HashMap.union m1 m2

-- | Creates a field parser for the top-level "node" field in the QueryRoot.
--
-- It exepcts one argument, the node id. It looks for the targeted table in the
-- 'NodeMap' returned by 'nodeInterface', and, if successful, attempts to craft
-- a corresponding 'QueryRootField' that will extract the requested row.
nodeField ::
  forall m n.
  (MonadError QErr m, P.MonadMemoize m, P.MonadParse n) =>
  SourceCache ->
  SchemaContext ->
  Options.SchemaOptions ->
  m (P.FieldParser n (IR.QueryRootField IR.UnpreparedValue))
nodeField sourceCache context options = do
  let idDescription = G.Description "A globally unique id"
      idArgument = P.field Name._id (Just idDescription) P.identifier
      stringifyNumbers = Options.soStringifyNumbers options
  nodeObject <- case scSchemaKind context of
    HasuraSchema -> throw500 "internal error: the node field should only be built for the Relay schema"
    RelaySchema nodeBuilder -> runNodeBuilder nodeBuilder context options
  pure
    $ P.subselection Name._node Nothing idArgument nodeObject
    `P.bindField` \(ident, parseds) -> do
      nodeId <- parseNodeId ident
      case nodeId of
        NodeIdV1 (V1NodeId tableName pKeys) -> do
          -- Node id V1.
          --
          -- We don't have the source name in a V1 node; we attempt all of them
          -- and pick the first one we find; there is a risk we might pick the
          -- wrong one if two tables with the same name exist in different
          -- sources! It is, however, unlikely; the engine emits V2 IDs, meaning
          -- if ever encounter a V1 ID it means it has been manually entered bya
          -- user, saved from an older version of the engine?
          let matchingTables = flip mapMaybe (HashMap.keys sourceCache) \sourceName ->
                findNode @('Postgres 'Vanilla) sourceName tableName parseds
          case matchingTables of
            [nodeValue] -> createRootField stringifyNumbers tableName nodeValue pKeys
            [] -> throwInvalidNodeId $ "no such table found: " <> toErrorValue tableName
            l ->
              throwInvalidNodeId
                $ "this V1 node id matches more than one table across different sources: "
                <> toErrorValue tableName
                <> " exists in sources "
                <> toErrorValue (_siName . nvSourceInfo <$> l)
        NodeIdV2 nodev2 ->
          -- Node id V2.
          --
          -- We have the source name and table name, we can extract the relevant
          -- info directly.
          AB.dispatchAnyBackend @Backend nodev2 \(V2NodeId sourceName tableName pKeys :: V2NodeId b) -> do
            nodeValue <-
              findNode @b sourceName tableName parseds
                `onNothing` throwInvalidNodeId ("no table " <> toErrorValue tableName <> " found in source " <> toErrorValue sourceName)
            createRootField stringifyNumbers tableName nodeValue pKeys
  where
    throwInvalidNodeId :: ErrorMessage -> n a
    throwInvalidNodeId t = P.withKey (J.Key "args") $ P.withKey (J.Key "id") $ P.parseError $ "invalid node id: " <> t

    parseNodeId :: Text -> n NodeId
    parseNodeId = either (throwInvalidNodeId . toErrorMessage . T.pack) pure . J.eitherDecode . base64Decode

    -- Given all the node id information about a table, and the extracted
    -- 'NodeInfo', craft the top-level query. This relies on the assumption
    -- that all backends that support relay use the same IR for single row
    -- selection.
    createRootField ::
      forall b.
      (Backend b) =>
      Options.StringifyNumbers ->
      TableName b ->
      NodeInfo b ->
      NESeq.NESeq J.Value ->
      n (IR.QueryRootField IR.UnpreparedValue)
    createRootField stringifyNumbers tableName (NodeInfo sourceInfo perms pKeys fields) columnValues = do
      whereExp <- buildNodeIdBoolExp (getter $ _siConfiguration sourceInfo) perms columnValues pKeys
      pure
        $ IR.RFDB (_siName sourceInfo)
        $ AB.mkAnyBackend
        $ IR.SourceConfigWith (_siConfiguration sourceInfo) Nothing
        $ IR.QDBR
        $ IR.QDBSingleRow
        $ IR.AnnSelectG
          { IR._asnFields = fields,
            IR._asnFrom = IR.FromTable tableName,
            IR._asnPerm = tablePermissionsInfo perms,
            IR._asnArgs =
              IR.SelectArgs
                { IR._saWhere = Just whereExp,
                  IR._saOrderBy = Nothing,
                  IR._saLimit = Nothing,
                  IR._saOffset = Nothing,
                  IR._saDistinct = Nothing
                },
            IR._asnStrfyNum = stringifyNumbers,
            IR._asnNamingConvention = Just $ _rscNamingConvention $ _siCustomization sourceInfo
          }

    -- Craft the 'where' condition of the query by making an `AEQ` entry for
    -- each primary key. This might fail if the given node id doesn't exactly
    -- have a valid entry for each primary key.
    buildNodeIdBoolExp ::
      (Backend b) =>
      ScalarTypeParsingContext b ->
      SelPermInfo b ->
      NESeq.NESeq J.Value ->
      NESeq.NESeq (ColumnInfo b) ->
      n (IR.AnnBoolExp b (IR.UnpreparedValue b))
    buildNodeIdBoolExp scalarTypeParsingContext selectPermissions columnValues pkeyColumns = do
      let firstPkColumn NESeq.:<|| remainingPkColumns = pkeyColumns
          firstColumnValue NESeq.:<|| remainingColumns = columnValues
          (nonAlignedPkColumns, nonAlignedColumnValues, alignedTuples) =
            partitionThese $ toList $ align remainingPkColumns remainingColumns

      unless (null nonAlignedPkColumns)
        $ throwInvalidNodeId
        $ "primary key columns "
        <> toErrorValue (map ciColumn nonAlignedPkColumns)
        <> " are missing"

      unless (null nonAlignedColumnValues)
        $ throwInvalidNodeId
        $ "unexpected column values "
        <> toErrorValue nonAlignedColumnValues

      let allTuples = (firstPkColumn, firstColumnValue) : alignedTuples
      IR.BoolAnd <$> for allTuples \(columnInfo, columnValue) -> do
        let columnType = ciType columnInfo
        let redactionExp = fromMaybe IR.NoRedaction $ getRedactionExprForColumn selectPermissions (ciColumn columnInfo)
        parsedValue <-
          parseScalarValueColumnTypeWithContext scalarTypeParsingContext columnType columnValue `onLeft` \e ->
            P.parseErrorWith P.ParseFailed $ "value of column " <> toErrorValue (ciColumn columnInfo) <> " in node id: " <> toErrorMessage (qeError e)
        pure
          $ IR.BoolField
          $ IR.AVColumn
            columnInfo
            redactionExp
            [IR.AEQ IR.NonNullableComparison $ IR.UVParameter IR.FreshVar $ ColumnValue columnType parsedValue]
