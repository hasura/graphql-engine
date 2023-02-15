module Hasura.CustomReturnType.Common
  ( toFieldInfo,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.Text.Extended (ToTxt (toTxt))
import Hasura.CustomReturnType (CustomReturnType (..))
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.Column (ColumnInfo (..), ColumnMutability (..), ColumnType (..))
import Hasura.RQL.Types.Table (FieldInfo (..))
import Language.GraphQL.Draft.Syntax qualified as G

toFieldInfo :: forall b. (Backend b) => CustomReturnType b -> Maybe [FieldInfo b]
toFieldInfo customReturnType =
  traverseWithIndex
    (\i -> fmap FIColumn . customTypeToColumnInfo i)
    (HashMap.toList (crtColumns customReturnType))
  where
    traverseWithIndex :: (Applicative m) => (Int -> aa -> m bb) -> [aa] -> m [bb]
    traverseWithIndex f = zipWithM f [0 ..]

    customTypeToColumnInfo :: Int -> (Column b, ScalarType b) -> Maybe (ColumnInfo b)
    customTypeToColumnInfo i (column, scalarType) = do
      name <- G.mkName (toTxt column)
      pure $
        ColumnInfo
          { ciColumn = column,
            ciName = name,
            ciPosition = i,
            ciType = ColumnScalar scalarType,
            ciIsNullable = False,
            ciDescription = Nothing,
            ciMutability = ColumnMutability {_cmIsInsertable = False, _cmIsUpdatable = False}
          }
