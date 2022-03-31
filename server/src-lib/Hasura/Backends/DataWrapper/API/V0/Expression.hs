{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

--
module Hasura.Backends.DataWrapper.API.V0.Expression
  ( Expression (..),
    Operator (..),
  )
where

import Autodocodec.Extended
import Autodocodec.OpenAPI ()
import Control.Lens.TH (makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Hasura.Backends.DataWrapper.API.V0.Column qualified as API.V0
import Hasura.Backends.DataWrapper.API.V0.Scalar.Value qualified as API.V0.Scalar
import Hasura.Incremental (Cacheable)
import Hasura.Prelude

--------------------------------------------------------------------------------

-- | A serializable representation of query operators.
data Operator
  = LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  deriving stock (Data, Eq, Generic, Ord, Show, Enum, Bounded)
  deriving anyclass (Cacheable, Hashable, NFData)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec Operator

instance HasCodec Operator where
  codec =
    named "Operator" $
      disjointStringConstCodec
        [ (LessThan, "less_than"),
          (LessThanOrEqual, "less_than_or_equal"),
          (GreaterThan, "greater_than"),
          (GreaterThanOrEqual, "greater_than_or_equal")
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
  | IsNotNull (ValueWrapper "expression" Expression)
  | Column (ValueWrapper "column" API.V0.ColumnName)
  | Equal (ValueWrapper2 "left" Expression "right" Expression)
  | NotEqual (ValueWrapper2 "left" Expression "right" Expression)
  | ApplyOperator (ValueWrapper3 "operator" Operator "left" Expression "right" Expression)
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, Hashable, NFData)

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
          TypeAlternative "IsNotNullExpression" "is_not_null" _IsNotNull,
          TypeAlternative "ColumnExpression" "column" _Column,
          TypeAlternative "EqualExpression" "equal" _Equal,
          TypeAlternative "NotEqualExpression" "not_equal" _NotEqual,
          TypeAlternative "ApplyOperatorExpression" "op" _ApplyOperator
        ]

deriving via Autodocodec Expression instance FromJSON Expression

deriving via Autodocodec Expression instance ToJSON Expression

deriving via Autodocodec Expression instance ToSchema Expression
