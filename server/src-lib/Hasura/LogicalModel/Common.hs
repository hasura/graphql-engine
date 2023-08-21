module Hasura.LogicalModel.Common
  ( columnsFromFields,
    logicalModelFieldsToFieldInfo,
    getSelPermInfoForLogicalModel,
    logicalModelPermissions,
  )
where

import Data.Bifunctor (bimap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Text.Extended (ToTxt (toTxt))
import Hasura.GraphQL.Schema.Common
  ( partialSQLExpToUnpreparedValue,
  )
import Hasura.LogicalModel.Cache
import Hasura.LogicalModel.NullableScalarType (NullableScalarType (..))
import Hasura.LogicalModel.Types (LogicalModelField (..), LogicalModelType (..), LogicalModelTypeArray (..), LogicalModelTypeReference (..), LogicalModelTypeScalar (..))
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.IR.BoolExp (AnnRedactionExp (..), gBoolExpTrue)
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.Column (ColumnInfo (..), ColumnMutability (..), ColumnType (..), NestedArrayInfo (..), NestedObjectInfo (..), StructuredColumnInfo (..), fromCol)
import Hasura.RQL.Types.Permission (AllowedRootFields (..))
import Hasura.RQL.Types.Roles (RoleName, adminRoleName)
import Hasura.Table.Cache (FieldInfo (..), FieldInfoMap, RolePermInfo (..), SelPermInfo (..))
import Language.GraphQL.Draft.Syntax qualified as G

-- | build select permissions for logical model
logicalModelPermissions ::
  (Backend b) =>
  LogicalModelInfo b ->
  RoleName ->
  Maybe (IR.TablePermG b (IR.UnpreparedValue b))
logicalModelPermissions logicalModel roleName = do
  getSelPermInfoForLogicalModel roleName logicalModel <&> \selectPermissions ->
    IR.TablePerm
      { IR._tpFilter = fmap partialSQLExpToUnpreparedValue <$> spiFilter selectPermissions,
        IR._tpLimit = spiLimit selectPermissions
      }

columnsFromFields ::
  InsOrdHashMap.InsOrdHashMap k (LogicalModelField b) ->
  InsOrdHashMap.InsOrdHashMap k (NullableScalarType b)
columnsFromFields =
  InsOrdHashMap.mapMaybe
    ( \case
        LogicalModelField
          { lmfType =
              LogicalModelTypeScalar
                ( LogicalModelTypeScalarC
                    { lmtsScalar = nstType,
                      lmtsNullable = nstNullable
                    }
                  ),
            lmfDescription = nstDescription
          } ->
            Just (NullableScalarType {..})
        _ -> Nothing
    )

logicalModelFieldsToFieldInfo ::
  forall b.
  (Backend b) =>
  InsOrdHashMap.InsOrdHashMap (Column b) (LogicalModelField b) ->
  FieldInfoMap (FieldInfo b)
logicalModelFieldsToFieldInfo =
  HashMap.fromList
    . fmap (bimap (fromCol @b) FIColumn)
    . catMaybes
    . zipWith (\i (column, lmf) -> (column,) <$> logicalModelFieldToStructuredColumnInfo i lmf) [0 ..]
    . InsOrdHashMap.toList
  where
    logicalModelFieldToStructuredColumnInfo :: Int -> LogicalModelField b -> Maybe (StructuredColumnInfo b)
    logicalModelFieldToStructuredColumnInfo i LogicalModelField {..} = go lmfType
      where
        go = \case
          LogicalModelTypeScalar LogicalModelTypeScalarC {..} -> do
            name <- G.mkName (toTxt lmfName)
            pure
              $ SCIScalarColumn
              $ ColumnInfo
                { ciColumn = lmfName,
                  ciName = name,
                  ciPosition = i,
                  ciType = ColumnScalar lmtsScalar,
                  ciIsNullable = lmtsNullable,
                  ciDescription = G.Description <$> lmfDescription,
                  ciMutability = ColumnMutability {_cmIsInsertable = False, _cmIsUpdatable = False}
                }
          LogicalModelTypeArray LogicalModelTypeArrayC {..} -> do
            supportsNestedObjects <- eitherToMaybe $ backendSupportsNestedObjects @b
            arrayColumnInfo <- go lmtaArray
            pure
              $ SCIArrayColumn
              $ NestedArrayInfo
                { _naiSupportsNestedArrays = supportsNestedObjects,
                  _naiIsNullable = lmtaNullable,
                  _naiColumnInfo = arrayColumnInfo
                }
          LogicalModelTypeReference LogicalModelTypeReferenceC {..} -> do
            supportsNestedObjects <- eitherToMaybe $ backendSupportsNestedObjects @b
            name <- G.mkName (toTxt lmfName)
            pure
              $ SCIObjectColumn
              $ NestedObjectInfo
                { _noiSupportsNestedObjects = supportsNestedObjects,
                  _noiColumn = lmfName,
                  _noiName = name,
                  _noiType = lmtrReference,
                  _noiIsNullable = lmtrNullable,
                  _noiDescription = G.Description <$> lmfDescription,
                  _noiMutability = ColumnMutability {_cmIsInsertable = False, _cmIsUpdatable = False}
                }

getSelPermInfoForLogicalModel ::
  (Backend b) =>
  RoleName ->
  LogicalModelInfo b ->
  Maybe (SelPermInfo b)
getSelPermInfoForLogicalModel role logicalModel =
  if role == adminRoleName
    then Just $ mkAdminSelPermInfo logicalModel
    else HashMap.lookup role (_lmiPermissions logicalModel) >>= _permSel

mkAdminSelPermInfo :: (Backend b) => LogicalModelInfo b -> SelPermInfo b
mkAdminSelPermInfo LogicalModelInfo {..} =
  SelPermInfo
    { spiCols = HashMap.fromList $ (,NoRedaction) <$> InsOrdHashMap.keys _lmiFields,
      spiComputedFields = mempty,
      spiFilter = gBoolExpTrue,
      spiLimit = Nothing,
      spiAllowAgg = True,
      spiRequiredHeaders = mempty,
      spiAllowedQueryRootFields = ARFAllowAllRootFields,
      spiAllowedSubscriptionRootFields = ARFAllowAllRootFields
    }
