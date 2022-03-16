{-# LANGUAGE StandaloneKindSignatures #-}

module Hasura.Backends.DataWrapper.IR.Name
  ( Name (..),
    NameType (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Kind (Type)
import Data.Text.Extended (ToTxt)
import Hasura.Backends.DataWrapper.API qualified as API
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Witch

--------------------------------------------------------------------------------

-- | A tagged, opaque wrapper around 'Text' that provides a number of derived
-- derived instances (primarily as required by the @Backend@ typeclass).
--
-- This wrapper is indexed by 'NameType' so that different "names" can be
-- represented as semantically distinct types without the boilerplate of
-- actually defining these wrappers separately.
type Name :: NameType -> Type
newtype Name ty = Name {unName :: Text}
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving newtype
    ( Cacheable,
      FromJSON,
      FromJSONKey,
      Hashable,
      NFData,
      ToJSON,
      ToJSONKey,
      ToTxt
    )

instance From API.TableName (Name 'Table) where
  from (API.TableName n) = coerce @Text @(Name 'Table) n

instance From API.ColumnName (Name 'Column) where
  from (API.ColumnName n) = coerce @Text @(Name 'Column) n

-- | The "type" of "name" that the 'Name' type is meant to provide a textual
-- representation for.
--
-- In other words: an enumeration of all the types for which 'Name' acts as a
-- shared abstraction.
data NameType
  = Column
  | Function
  | Table
