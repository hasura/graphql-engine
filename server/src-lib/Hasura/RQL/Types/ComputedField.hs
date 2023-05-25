{-# LANGUAGE TemplateHaskell #-}

-- |
-- Description: Schema cache types related to computed field
module Hasura.RQL.Types.ComputedField
  ( ComputedFieldFunction (..),
    ComputedFieldInfo (..),
    ComputedFieldName (..),
    CustomFunctionNames (..),
    FunctionTrackedAs (..),
    cfiDescription,
    cfiFunction,
    cfiName,
    cfiReturnType,
    cfiXComputedFieldInfo,
    computedFieldNameToText,
    fromComputedField,
    onlyNumComputedFields,
    isNumComputedField,
    onlyComparableComputedFields,
    isComparableComputedField,
    removeComputedFieldsReturningExistingTable,
  )
where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Sequence qualified as Seq
import Hasura.Backends.Postgres.SQL.Types hiding (FunctionName, TableName, isComparableType, isNumType)
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField.Name (ComputedFieldName (..), computedFieldNameToText, fromComputedField)
import Language.GraphQL.Draft.Syntax (Name)

data FunctionTrackedAs (b :: BackendType)
  = FTAComputedField ComputedFieldName SourceName (TableName b)
  | FTACustomFunction CustomFunctionNames
  deriving (Generic)

-- | The function name and input arguments name for the "args" field parser.
--
-- > function_name(args: args_name)
data CustomFunctionNames = CustomFunctionNames
  { cfnFunctionName :: Name,
    cfnArgsName :: Name
  }
  deriving (Show, Eq, Generic)

deriving instance (Backend b) => Show (FunctionTrackedAs b)

deriving instance (Backend b) => Eq (FunctionTrackedAs b)

data ComputedFieldFunction (b :: BackendType) = ComputedFieldFunction
  { _cffName :: FunctionName b,
    _cffInputArgs :: Seq.Seq (FunctionArgument b),
    _cffComputedFieldImplicitArgs :: ComputedFieldImplicitArguments b,
    _cffDescription :: Maybe PGDescription
  }
  deriving (Generic)

deriving instance (Backend b) => Show (ComputedFieldFunction b)

deriving instance (Backend b) => Eq (ComputedFieldFunction b)

deriving instance (Backend b) => Ord (ComputedFieldFunction b)

instance (Backend b) => NFData (ComputedFieldFunction b)

instance (Backend b) => Hashable (ComputedFieldFunction b)

instance (Backend b) => ToJSON (ComputedFieldFunction b) where
  toJSON = genericToJSON hasuraJSON

data ComputedFieldInfo (b :: BackendType) = ComputedFieldInfo
  { _cfiXComputedFieldInfo :: XComputedField b,
    _cfiName :: ComputedFieldName,
    _cfiFunction :: ComputedFieldFunction b,
    _cfiReturnType :: ComputedFieldReturn b,
    _cfiDescription :: Maybe Text
  }
  deriving (Generic)

deriving instance (Backend b) => Eq (ComputedFieldInfo b)

deriving instance (Backend b) => Ord (ComputedFieldInfo b)

deriving instance (Backend b) => Show (ComputedFieldInfo b)

instance (Backend b) => NFData (ComputedFieldInfo b)

instance (Backend b) => Hashable (ComputedFieldInfo b)

instance (Backend b) => ToJSON (ComputedFieldInfo b) where
  -- spelling out the JSON instance in order to skip the Trees That Grow field
  toJSON (ComputedFieldInfo _ name func tp description) =
    object ["name" .= name, "function" .= func, "return_type" .= tp, "description" .= description]

-- | Return all the computed fields in the given list that have numeric types.
onlyNumComputedFields :: forall b. (Backend b) => [ComputedFieldInfo b] -> [ComputedFieldInfo b]
onlyNumComputedFields = filter isNumComputedField

-- | Check whether a computed field has a numeric type.
isNumComputedField :: forall b. (Backend b) => ComputedFieldInfo b -> Bool
isNumComputedField cfi = case computedFieldReturnType @b (_cfiReturnType cfi) of
  ReturnsScalar t -> isNumType @b t
  _ -> False

-- | Return all the computed fields in the given list that have numeric types.
onlyComparableComputedFields :: forall b. (Backend b) => [ComputedFieldInfo b] -> [ComputedFieldInfo b]
onlyComparableComputedFields = filter isComparableComputedField

-- | Check whether a computed field has a numeric type.
isComparableComputedField :: forall b. (Backend b) => ComputedFieldInfo b -> Bool
isComparableComputedField cfi = case computedFieldReturnType @b (_cfiReturnType cfi) of
  ReturnsScalar t -> isComparableType @b t
  _ -> False

$(makeLenses ''ComputedFieldInfo)

-- | Filter computed fields not returning rows of existing table
removeComputedFieldsReturningExistingTable ::
  forall backend.
  (Backend backend) =>
  [ComputedFieldInfo backend] ->
  [ComputedFieldInfo backend]
removeComputedFieldsReturningExistingTable =
  filter (not . has _ReturnsTable . computedFieldReturnType @backend . _cfiReturnType)
