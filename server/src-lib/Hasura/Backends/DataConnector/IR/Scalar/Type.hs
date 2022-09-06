{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Backends.DataConnector.IR.Scalar.Type
  ( Type (..),
    fromGQLType,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Text.Extended (ToTxt (..))
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Base.ErrorValue qualified as ErrorValue
import Hasura.Base.ToErrorValue
import Hasura.GraphQL.Parser.Name.TypeSystem
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as GQL
import Witch qualified

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
  | Custom Text
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

instance ToErrorValue Type where
  toErrorValue = ErrorValue.squote . tshow

instance Witch.From API.Type Type where
  from = \case
    API.StringTy -> String
    API.NumberTy -> Number
    API.BoolTy -> Bool
    API.CustomTy name -> Custom name

instance Witch.From Type API.Type where
  from = \case
    String -> API.StringTy
    Number -> API.NumberTy
    Bool -> API.BoolTy
    Custom name -> API.CustomTy name

fromGQLType :: GQL.Name -> Type
fromGQLType typeName =
  if
      | typeName == _String -> String
      | typeName == _Int -> Number
      | typeName == _Float -> Number
      | typeName == _Boolean -> Bool
      | otherwise -> Custom $ GQL.unName typeName
