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
    removeComputedFieldsReturningExistingTable,
  )
where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Sequence qualified as Seq
import Data.Text.Extended
import Data.Text.NonEmpty (NonEmptyText (..))
import Database.PG.Query qualified as Q
import Hasura.Backends.Postgres.SQL.Types hiding (FunctionName, TableName)
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.SQL.Backend
import Language.GraphQL.Draft.Syntax (Name)

newtype ComputedFieldName = ComputedFieldName {unComputedFieldName :: NonEmptyText}
  deriving (Show, Eq, Ord, NFData, FromJSON, ToJSON, ToJSONKey, Q.ToPrepArg, ToTxt, Hashable, Q.FromCol, Generic, Cacheable)

computedFieldNameToText :: ComputedFieldName -> Text
computedFieldNameToText = unNonEmptyText . unComputedFieldName

fromComputedField :: ComputedFieldName -> FieldName
fromComputedField = FieldName . computedFieldNameToText

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

deriving instance Backend b => Show (FunctionTrackedAs b)

deriving instance Backend b => Eq (FunctionTrackedAs b)

data ComputedFieldFunction (b :: BackendType) = ComputedFieldFunction
  { _cffName :: !(FunctionName b),
    _cffInputArgs :: !(Seq.Seq (FunctionArgument b)),
    _cffComputedFieldImplicitArgs :: !(ComputedFieldImplicitArguments b),
    _cffDescription :: !(Maybe PGDescription)
  }
  deriving (Generic)

deriving instance (Backend b) => Show (ComputedFieldFunction b)

deriving instance (Backend b) => Eq (ComputedFieldFunction b)

instance (Backend b) => Cacheable (ComputedFieldFunction b)

instance (Backend b) => NFData (ComputedFieldFunction b)

instance (Backend b) => Hashable (ComputedFieldFunction b)

instance (Backend b) => ToJSON (ComputedFieldFunction b) where
  toJSON = genericToJSON hasuraJSON

data ComputedFieldInfo (b :: BackendType) = ComputedFieldInfo
  { _cfiXComputedFieldInfo :: !(XComputedField b),
    _cfiName :: !ComputedFieldName,
    _cfiFunction :: !(ComputedFieldFunction b),
    _cfiReturnType :: !(ComputedFieldReturn b),
    _cfiDescription :: !(Maybe Text)
  }
  deriving (Generic)

deriving instance (Backend b) => Eq (ComputedFieldInfo b)

deriving instance (Backend b) => Show (ComputedFieldInfo b)

instance (Backend b) => NFData (ComputedFieldInfo b)

instance (Backend b) => Cacheable (ComputedFieldInfo b)

instance (Backend b) => Hashable (ComputedFieldInfo b)

instance (Backend b) => ToJSON (ComputedFieldInfo b) where
  -- spelling out the JSON instance in order to skip the Trees That Grow field
  toJSON (ComputedFieldInfo _ name func tp description) =
    object ["name" .= name, "function" .= func, "return_type" .= tp, "description" .= description]

$(makeLenses ''ComputedFieldInfo)

-- | Filter computed fields not returning rows of existing table
removeComputedFieldsReturningExistingTable ::
  forall backend.
  (Backend backend) =>
  [ComputedFieldInfo backend] ->
  [ComputedFieldInfo backend]
removeComputedFieldsReturningExistingTable =
  filter (not . has _ReturnsTable . computedFieldReturnType @backend . _cfiReturnType)
