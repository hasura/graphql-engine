module Hasura.Server.Migrate.Version (CatalogVersion (..)) where

import Data.List (isPrefixOf)
import Hasura.Prelude
import Language.Haskell.TH.Lift (Lift)

-- | Represents the catalog version. This is stored in the database and then
-- compared with the latest version on startup.
data CatalogVersion
  = -- | A typical catalog version.
    CatalogVersion Int
  | -- | Maintained for compatibility with catalog version 0.8.
    CatalogVersion08
  deriving stock (Eq, Lift)

instance Ord CatalogVersion where
  compare = compare `on` toFloat
    where
      toFloat :: CatalogVersion -> Float
      toFloat (CatalogVersion v) = fromIntegral v
      toFloat CatalogVersion08 = 0.8

instance Enum CatalogVersion where
  toEnum = CatalogVersion
  fromEnum (CatalogVersion v) = v
  fromEnum CatalogVersion08 = error "Cannot enumerate unstable catalog versions."

instance Show CatalogVersion where
  show (CatalogVersion v) = show v
  show CatalogVersion08 = "0.8"

instance Read CatalogVersion where
  readsPrec prec s
    | "0.8" `isPrefixOf` s =
      [(CatalogVersion08, drop 3 s)]
    | otherwise =
      map (first CatalogVersion) $ filter ((>= 0) . fst) $ readsPrec @Int prec s
