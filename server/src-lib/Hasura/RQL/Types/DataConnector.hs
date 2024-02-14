{-# LANGUAGE DeriveAnyClass #-}

module Hasura.RQL.Types.DataConnector
  ( DataConnectorName,
    unDataConnectorName,
    mkDataConnectorName,
  )
where

-- This file is only for Data Connector types that must be referenced in
-- `RQL.Types`

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Aeson qualified as J
import Data.Data (Typeable)
import Data.Text qualified as Text
import Data.Text.Extended (ToTxt (..))
import Data.Text.NonEmpty (NonEmptyText, mkNonEmptyTextUnsafe)
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as GQL
import Witch qualified

-- | Note: Currently you should not use underscores in this name.
--         This should be enforced in instances, and the `mkDataConnectorName`
--         smart constructor is available to assist.
newtype DataConnectorName = DataConnectorName {unDataConnectorName :: GQL.Name}
  deriving stock (Eq, Ord, Show, Typeable, Generic)
  deriving newtype (ToJSON, FromJSONKey, ToJSONKey, Hashable, ToTxt)
  deriving anyclass (NFData)

instance FromJSON DataConnectorName where
  parseJSON v = (`onLeft` fail) =<< (mkDataConnectorName <$> J.parseJSON v)

mkDataConnectorName :: GQL.Name -> Either String DataConnectorName
mkDataConnectorName n =
  if ('_' `Text.elem` GQL.unName n)
    then -- Could return other errors in future.
      Left "DataConnectorName may not contain underscores."
    else Right (DataConnectorName n)

instance Witch.From DataConnectorName NonEmptyText where
  from = mkNonEmptyTextUnsafe . GQL.unName . unDataConnectorName -- mkNonEmptyTextUnsafe is safe here since GQL.Name is never empty

instance Witch.From DataConnectorName Text where
  from = GQL.unName . unDataConnectorName
