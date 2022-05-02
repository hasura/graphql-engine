{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

--
module Hasura.Backends.DataConnector.API.V0.Expression
  ( Expression (..),
    Operator (..),
  )
where

import Autodocodec.Extended
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Control.Lens.TH (makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.Column qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Scalar.Value qualified as API.V0.Scalar
import Prelude

--------------------------------------------------------------------------------

-- | A serializable representation of query operators.
data Operator
  = LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  | Equal
  deriving stock (Data, Eq, Generic, Ord, Show, Enum, Bounded)
  deriving anyclass (Hashable, NFData)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec Operator

instance HasCodec Operator where
  codec =
    named "Operator" $
      disjointStringConstCodec
        [ (LessThan, "less_than"),
          (LessThanOrEqual, "less_than_or_equal"),
          (GreaterThan, "greater_than"),
          (GreaterThanOrEqual, "greater_than_or_equal"),
          (Equal, "equal")
        ]

--------------------------------------------------------------------------------

-- | A serializable representation of query expressions.
data Expression
  = -- | A constant 'Scalar.Value'.
    Literal (ValueWrapper "value" API.V0.Scalar.Value)
  | In (ValueWrapper2 "expression" Expression "values" [API.V0.Scalar.Value])
  | And (ValueWrapper "expressions" [Expression])
  | Or (ValueWrapper "expressions" [Expression])
  | Not (ValueWrapper "expression" Expression)
  | IsNull (ValueWrapper "expression" Expression)
  | Column (ValueWrapper "column" API.V0.ColumnName)
  | ApplyOperator (ValueWrapper3 "operator" Operator "left" Expression "right" Expression)
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Hashable, NFData)

$(makePrisms ''Expression)

instance HasCodec Expression where
  codec =
    named "Expression" $
      sumTypeCodec
        [ TypeAlternative "LiteralExpression" "literal" _Literal,
          TypeAlternative "InExpression" "in" _In,
          TypeAlternative "AndExpression" "and" _And,
          TypeAlternative "OrExpression" "or" _Or,
          TypeAlternative "NotExpression" "not" _Not,
          TypeAlternative "IsNullExpression" "is_null" _IsNull,
          TypeAlternative "ColumnExpression" "column" _Column,
          TypeAlternative "ApplyOperatorExpression" "op" _ApplyOperator
        ]

deriving via Autodocodec Expression instance FromJSON Expression

deriving via Autodocodec Expression instance ToJSON Expression

deriving via Autodocodec Expression instance ToSchema Expression
