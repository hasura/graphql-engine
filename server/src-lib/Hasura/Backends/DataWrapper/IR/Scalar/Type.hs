{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Backends.DataWrapper.IR.Scalar.Type
  ( Type (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Text.Extended (ToTxt (..))
import Hasura.Backends.DataWrapper.API qualified as API
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Witch

--------------------------------------------------------------------------------

-- | Types of scalar values
--
-- Used to specify the domain of legal values for a @Column@.
--
-- NOTE: This type shouldn't _need_ ser/de instances, but they're imposed by
-- the 'Backend' class.
--
-- XXX: Should we add a @Nullable _ :: Type@ constructor instead of using an
-- @isNullable@ flag in @Column@?
data Type
  = String
  | Number
  | Bool
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass
    ( Cacheable,
      FromJSON,
      FromJSONKey,
      Hashable,
      NFData,
      ToJSON,
      ToJSONKey
    )

instance ToTxt Type where
  toTxt = tshow

instance From API.Type Type where
  from = \case
    API.StringTy -> String
    API.NumberTy -> Number
    API.BoolTy -> Bool

instance From Type API.Type where
  from = \case
    String -> API.StringTy
    Number -> API.NumberTy
    Bool -> API.BoolTy
