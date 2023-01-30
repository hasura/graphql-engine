module Harness.Test.FixtureName
  ( backendTypesForFixture,
    FixtureName (..),
  )
where

import Data.Set qualified as S
import Harness.Test.BackendType
import Hasura.Prelude hiding (log)

-- | A name describing the given context.
data FixtureName
  = Backend BackendTypeConfig
  | RemoteGraphQLServer
  | Combine FixtureName FixtureName

backendTypesForFixture :: FixtureName -> S.Set BackendType
backendTypesForFixture (Backend be) = S.singleton (backendType be)
backendTypesForFixture RemoteGraphQLServer = mempty
backendTypesForFixture (Combine a b) =
  backendTypesForFixture a <> backendTypesForFixture b

instance Show FixtureName where
  show (Backend backend) = show (backendType backend)
  show RemoteGraphQLServer = "RemoteGraphQLServer"
  show (Combine name1 name2) = show name1 ++ "-" ++ show name2
