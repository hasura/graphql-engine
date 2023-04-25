module Hasura.LogicalModel.Common
  ( toFieldInfo,
    columnsFromFields,
    logicalModelFieldsToFieldInfo,
  )
where

import Data.Bifunctor (bimap)
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.Text.Extended (ToTxt (toTxt))
import Hasura.LogicalModel.Types (LogicalModelField (..))
import Hasura.NativeQuery.Types (NullableScalarType (..))
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.Column (ColumnInfo (..), ColumnMutability (..), ColumnType (..), fromCol)
import Hasura.RQL.Types.Table (FieldInfo (..), FieldInfoMap)
import Language.GraphQL.Draft.Syntax qualified as G

columnsFromFields ::
  InsOrd.InsOrdHashMap k (LogicalModelField b) ->
  InsOrd.InsOrdHashMap k (NullableScalarType b)
columnsFromFields =
  InsOrd.mapMaybe
    ( \case
        LogicalModelScalarField
          { lmfType = nstType,
            lmfNullable = nstNullable,
            lmfDescription = nstDescription
          } ->
            Just (NullableScalarType {..})
        _ -> Nothing
    )

toFieldInfo :: forall b. (Backend b) => InsOrd.InsOrdHashMap (Column b) (NullableScalarType b) -> Maybe [FieldInfo b]
toFieldInfo fields =
  traverseWithIndex
    (\i -> fmap FIColumn . logicalModelToColumnInfo i)
    (InsOrd.toList fields)

traverseWithIndex :: (Applicative m) => (Int -> aa -> m bb) -> [aa] -> m [bb]
traverseWithIndex f = zipWithM f [0 ..]

logicalModelToColumnInfo :: forall b. (Backend b) => Int -> (Column b, NullableScalarType b) -> Maybe (ColumnInfo b)
logicalModelToColumnInfo i (column, NullableScalarType {..}) = do
  name <- G.mkName (toTxt column)
  pure $
    ColumnInfo
      { ciColumn = column,
        ciName = name,
        ciPosition = i,
        ciType = ColumnScalar nstType,
        ciIsNullable = nstNullable,
        ciDescription = G.Description <$> nstDescription,
        ciMutability = ColumnMutability {_cmIsInsertable = False, _cmIsUpdatable = False}
      }

logicalModelFieldsToFieldInfo ::
  forall b.
  (Backend b) =>
  InsOrd.InsOrdHashMap (Column b) (LogicalModelField b) ->
  FieldInfoMap (FieldInfo b)
logicalModelFieldsToFieldInfo =
  Map.fromList
    . fmap (bimap (fromCol @b) FIColumn)
    . fromMaybe mempty
    . traverseWithIndex
      (\i (column, lmf) -> (,) column <$> logicalModelToColumnInfo i (column, lmf))
    . InsOrd.toList
    . columnsFromFields
