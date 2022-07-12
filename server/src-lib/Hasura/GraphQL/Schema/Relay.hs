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
import Data.Aeson.Extended qualified as J
import Data.Aeson.Types qualified as J
import Data.Align (align)
import Data.Has
import Data.HashMap.Strict.Extended qualified as Map
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Data.Text.Extended
import Data.These (partitionThese)
import Hasura.Base.Error
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Instances ()
import Hasura.GraphQL.Schema.Node
import Hasura.GraphQL.Schema.Parser (Kind (..), Parser, memoizeOn)
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Select
import Hasura.GraphQL.Schema.Table
import Hasura.GraphQL.Schema.Typename (withTypenameCustomization)
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.SchemaCache hiding (askTableInfo)
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.RQL.Types.Table
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend
import Language.GraphQL.Draft.Syntax qualified as G

-- | Constructs the parser for the node interface.
--
-- As mentioned in Note [Internal Relay HashMap], this function must parse an
-- incoming query for ANY potential matching table. Its resulting parser returns
-- a 'NodeMap': a container that, to a source name and a table name, associates
-- both the parsed fields and all the relevant table information required to
-- craft a request.
nodeInterface :: SourceCache -> NodeInterfaceParserBuilder
nodeInterface sourceCache = NodeInterfaceParserBuilder $ memoizeOn 'nodeInterface () do
  let idDescription = G.Description "A globally unique identifier"
      idField = P.selection_ Name._id (Just idDescription) P.identifier
      nodeInterfaceDescription = G.Description "An object with globally unique ID"
  tCase <- asks getter
  tables :: [Parser 'Output n (SourceName, AB.AnyBackend TableMap)] <-
    catMaybes . concat <$> for (Map.toList sourceCache) \(sourceName, anySourceInfo) ->
      AB.dispatchAnyBackendWithTwoConstraints @BackendSchema @BackendTableSelectSchema
        anySourceInfo
        \(sourceInfo :: SourceInfo b) ->
          for (Map.toList $ takeValidTables $ _siTables sourceInfo) \(tableName, tableInfo) -> runMaybeT do
            tablePkeyColumns <- hoistMaybe $ tableInfo ^? tiCoreInfo . tciPrimaryKey . _Just . pkColumns
            selectPermissions <- MaybeT $ tableSelectPermissions tableInfo
            annotatedFieldsParser <-
              MaybeT $
                withTypenameCustomization
                  (mkCustomizedTypename (_scTypeNames $ _siCustomization sourceInfo) tCase)
                  (tableSelectionSet sourceInfo tableInfo)
            pure $
              annotatedFieldsParser <&> \fields ->
                ( sourceName,
                  AB.mkAnyBackend $
                    TableMap $
                      Map.singleton tableName $
                        NodeInfo (_siConfiguration sourceInfo) selectPermissions tablePkeyColumns fields
                )
  pure $
    Map.fromListWith fuseAnyMaps
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
      AB.composeAnyBackend @Backend fuseMaps m1 m2 $
        error "panic: two tables of a different backend type within the same source"

    fuseMaps :: forall b. Backend b => TableMap b -> TableMap b -> AB.AnyBackend TableMap
    fuseMaps (TableMap m1) (TableMap m2) = AB.mkAnyBackend @b $ TableMap $ Map.union m1 m2

-- | Creates a field parser for the top-level "node" field in the QueryRoot.
--
-- It exepcts one argument, the node id. It looks for the targeted table in the
-- 'NodeMap' returned by 'nodeInterface', and, if successful, attempts to craft
-- a corresponding 'QueryRootField' that will extract the requested row.
nodeField ::
  forall m n r.
  SourceCache ->
  MonadBuildSchemaBase r m n =>
  m (P.FieldParser n (IR.QueryRootField IR.UnpreparedValue))
nodeField sourceCache = do
  let idDescription = G.Description "A globally unique id"
      idArgument = P.field Name._id (Just idDescription) P.identifier
  stringifyNum <- retrieve soStringifyNum
  nodeObject <-
    retrieve scSchemaKind >>= \case
      HasuraSchema -> throw500 "internal error: the node field should only be built for the Relay schema"
      RelaySchema nodeBuilder -> runNodeBuilder nodeBuilder
  pure $
    P.subselection Name._node Nothing idArgument nodeObject `P.bindField` \(ident, parseds) -> do
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
          let matchingTables = flip mapMaybe (Map.keys sourceCache) \sourceName ->
                (sourceName,) <$> findNode @('Postgres 'Vanilla) sourceName tableName parseds
          case matchingTables of
            [(sourceName, nodeValue)] -> createRootField stringifyNum sourceName tableName nodeValue pKeys
            [] -> throwInvalidNodeId $ "no such table found: " <>> tableName
            l ->
              throwInvalidNodeId $
                "this V1 node id matches more than one table across different sources: " <> tableName
                  <<> " exists in sources "
                  <> commaSeparated (fst <$> l)
        NodeIdV2 nodev2 ->
          -- Node id V2.
          --
          -- We have the source name and table name, we can extract the relevant
          -- info directly.
          AB.dispatchAnyBackend @Backend nodev2 \(V2NodeId sourceName tableName pKeys :: V2NodeId b) -> do
            nodeValue <-
              findNode @b sourceName tableName parseds
                `onNothing` throwInvalidNodeId ("no table " <> tableName <<> " found in source " <>> sourceName)
            createRootField stringifyNum sourceName tableName nodeValue pKeys
  where
    throwInvalidNodeId :: Text -> n a
    throwInvalidNodeId t = P.withKey (J.Key "args") $ P.withKey (J.Key "id") $ P.parseError $ "invalid node id: " <> t

    parseNodeId :: Text -> n NodeId
    parseNodeId = either (throwInvalidNodeId . T.pack) pure . J.eitherDecode . base64Decode

    -- Given all the node id information about a table, and the extracted
    -- 'NodeInfo', craft the top-level query. This relies on the assumption
    -- that all backends that support relay use the same IR for single row
    -- selection.
    createRootField ::
      Backend b =>
      StringifyNumbers ->
      SourceName ->
      TableName b ->
      NodeInfo b ->
      NESeq.NESeq J.Value ->
      n (IR.QueryRootField IR.UnpreparedValue)
    createRootField stringifyNum sourceName tableName (NodeInfo sourceConfig perms pKeys fields) columnValues = do
      whereExp <- buildNodeIdBoolExp columnValues pKeys
      pure $
        IR.RFDB sourceName $
          AB.mkAnyBackend $
            IR.SourceConfigWith sourceConfig Nothing $
              IR.QDBR $
                IR.QDBSingleRow $
                  IR.AnnSelectG
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
                      IR._asnStrfyNum = stringifyNum
                    }

    -- Craft the 'where' condition of the query by making an `AEQ` entry for
    -- each primary key. This might fail if the given node id doesn't exactly
    -- have a valid entry for each primary key.
    buildNodeIdBoolExp ::
      Backend b =>
      NESeq.NESeq J.Value ->
      NESeq.NESeq (ColumnInfo b) ->
      n (IR.AnnBoolExp b (IR.UnpreparedValue b))
    buildNodeIdBoolExp columnValues pkeyColumns = do
      let firstPkColumn NESeq.:<|| remainingPkColumns = pkeyColumns
          firstColumnValue NESeq.:<|| remainingColumns = columnValues
          (nonAlignedPkColumns, nonAlignedColumnValues, alignedTuples) =
            partitionThese $ toList $ align remainingPkColumns remainingColumns

      unless (null nonAlignedPkColumns) $
        throwInvalidNodeId $
          "primary key columns " <> dquoteList (map ciColumn nonAlignedPkColumns) <> " are missing"

      unless (null nonAlignedColumnValues) $
        throwInvalidNodeId $
          "unexpected column values " <> J.encodeToStrictText nonAlignedColumnValues

      let allTuples = (firstPkColumn, firstColumnValue) : alignedTuples
      IR.BoolAnd <$> for allTuples \(columnInfo, columnValue) -> do
        let columnType = ciType columnInfo
        parsedValue <-
          parseScalarValueColumnType columnType columnValue `onLeft` \e ->
            P.parseErrorWith ParseFailed $ "value of column " <> ciColumn columnInfo <<> " in node id: " <> qeError e
        pure $
          IR.BoolField $
            IR.AVColumn
              columnInfo
              [IR.AEQ True $ IR.UVParameter Nothing $ ColumnValue columnType parsedValue]
