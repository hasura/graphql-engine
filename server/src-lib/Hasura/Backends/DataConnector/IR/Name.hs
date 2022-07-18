{-# LANGUAGE StandaloneKindSignatures #-}

module Hasura.Backends.DataConnector.IR.Name
  ( Name (..),
    NameType (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Kind (Type)
import Data.Text.Extended (ToTxt)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Base.ErrorValue qualified as ErrorValue
import Hasura.Base.ToErrorValue
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Witch qualified

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

instance ToErrorValue (Name ty) where
  toErrorValue = ErrorValue.squote . unName

instance Witch.From API.TableName (Name 'Table) where
  from (API.TableName n) = Name n

instance Witch.From (Name 'Table) API.TableName where
  from (Name n) = API.TableName n

instance Witch.From API.ColumnName (Name 'Column) where
  from (API.ColumnName n) = Name n

instance Witch.From (Name 'Column) API.ColumnName where
  from (Name n) = API.ColumnName n

instance Witch.From API.RelationshipName (Name 'Relationship) where
  from (API.RelationshipName n) = Name n

instance Witch.From (Name 'Relationship) API.RelationshipName where
  from (Name n) = API.RelationshipName n

-- | The "type" of "name" that the 'Name' type is meant to provide a textual
-- representation for.
--
-- In other words: an enumeration of all the types for which 'Name' acts as a
-- shared abstraction.
data NameType
  = Column
  | Function
  | Table
  | Relationship
