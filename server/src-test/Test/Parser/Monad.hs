{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | This module defines the monads required to run parser tests.
--
-- Warning: a lot of the implementations are currently 'undefined'. As we write
-- more advanced tests, they might require implementations.
module Test.Parser.Monad
  ( ParserTestT (..),
    SchemaEnvironment,
    SchemaTestT (..),
  )
where

import Data.Aeson.Internal (JSONPath)
import Data.Has (Has (..))
import Data.Text qualified as T
import Hasura.Base.Error (Code, QErr)
import Hasura.GraphQL.Execute.Types (GraphQLQueryType (..))
import Hasura.GraphQL.Parser.Class (MonadParse (..), MonadSchema (..))
import Hasura.GraphQL.Parser.Schema (MkTypename (..))
import Hasura.GraphQL.Schema.Common (SchemaContext (..), SchemaOptions (..), ignoreRemoteRelationship)
import Hasura.Prelude
import Hasura.RQL.Types.Common (StringifyNumbers (LeaveNumbersAlone))
import Hasura.RQL.Types.Function (FunctionPermissionsCtx (..))
import Hasura.RQL.Types.RemoteSchema (RemoteSchemaPermsCtx (..))
import Hasura.RQL.Types.SourceCustomization (CustomizeRemoteFieldName, MkRootFieldName, NamingCase (..))
import Hasura.Session (RoleName, adminRoleName)
import Language.Haskell.TH.Syntax qualified as TH
import Test.Hspec

notImplemented :: String -> a
notImplemented location =
  error $ "Not implemented: Test.Parser.Monad." <> location

-- | Monad builder environment.
--
-- Parser functions generally have a return type of @m (Parser n)@. The @m@
-- parameter is mocked through 'SchemaTestT', which requires a bunch of 'Has'
-- instances, as well as a 'ReaderT' instance for environment
-- settings/configurations. This type repesents these settings.
--
-- SchemaEnvironment: currently void. This is subject to change if we require
-- more complex setup.
data SchemaEnvironment

instance Has NamingCase SchemaEnvironment where
  getter :: SchemaEnvironment -> NamingCase
  getter = const HasuraCase

  modifier :: (NamingCase -> NamingCase) -> SchemaEnvironment -> SchemaEnvironment
  modifier = notImplemented "modifier<Has NamingCase SchemaEnvironment>"

instance Has RoleName SchemaEnvironment where
  getter :: SchemaEnvironment -> RoleName
  getter = const adminRoleName

  modifier :: (RoleName -> RoleName) -> SchemaEnvironment -> SchemaEnvironment
  modifier = notImplemented "modifier<Has RoleName SchemaEnvironment>"

instance Has SchemaOptions SchemaEnvironment where
  getter :: SchemaEnvironment -> SchemaOptions
  getter =
    const
      SchemaOptions
        { soStringifyNum = LeaveNumbersAlone,
          soDangerousBooleanCollapse = False,
          soQueryType = QueryHasura,
          soFunctionPermsContext = FunctionPermissionsInferred,
          soRemoteSchemaPermsCtx = RemoteSchemaPermsDisabled,
          soOptimizePermissionFilters = False
        }

  modifier :: (SchemaOptions -> SchemaOptions) -> SchemaEnvironment -> SchemaEnvironment
  modifier = notImplemented "modifier<Has SchemaOptions SchemaEnvironment>"

instance Has SchemaContext SchemaEnvironment where
  getter :: SchemaEnvironment -> SchemaContext
  getter =
    const
      SchemaContext
        { scSourceCache = notImplemented "scSourceCache",
          scRemoteRelationshipParserBuilder = ignoreRemoteRelationship
        }

  modifier :: (SchemaContext -> SchemaContext) -> SchemaEnvironment -> SchemaEnvironment
  modifier = notImplemented "modifier<Has SchemaContext SchemaEnvironment>"

instance Has MkTypename SchemaEnvironment where
  getter :: SchemaEnvironment -> MkTypename
  getter = const (MkTypename id)

  modifier :: (MkTypename -> MkTypename) -> SchemaEnvironment -> SchemaEnvironment
  modifier = notImplemented "modifier<Has MkTypeName SchemaEnvironment>"

instance Has MkRootFieldName SchemaEnvironment where
  getter :: SchemaEnvironment -> MkRootFieldName
  getter = const mempty

  modifier :: (MkRootFieldName -> MkRootFieldName) -> SchemaEnvironment -> SchemaEnvironment
  modifier = notImplemented "modifier<Has MkRootFieldName SchemaEnvironment>"

instance Has CustomizeRemoteFieldName SchemaEnvironment where
  getter :: SchemaEnvironment -> CustomizeRemoteFieldName
  getter = notImplemented "getter<Has CustomizeRemoteFieldName SchemaEnvironment>"

  modifier :: (CustomizeRemoteFieldName -> CustomizeRemoteFieldName) -> SchemaEnvironment -> SchemaEnvironment
  modifier = notImplemented "modifier<Has CustomizeRemoteFieldName SchemaEnvironment>"

-------------------------------------------------------------------------------

-- | SchemaTestT
newtype SchemaTestT a = SchemaTestT a
  deriving stock (Functor)
  deriving (Applicative, Monad) via Identity

instance MonadError QErr SchemaTestT where
  throwError :: forall a. QErr -> SchemaTestT a
  throwError = notImplemented "throwError<MonadError QErr SchemaTestT>"

  catchError :: forall a. SchemaTestT a -> (QErr -> SchemaTestT a) -> SchemaTestT a
  catchError = notImplemented "catchError<MonadError QErr SchemaTestT>"

-- | Note this is not used because all the actual getters/setters for
-- SchemaEnvironment are @const X@, so these bottoms never actually get
-- evaluated.
instance MonadReader SchemaEnvironment SchemaTestT where
  ask :: SchemaTestT SchemaEnvironment
  ask = notImplemented "ask<MonadReader SchemaEnvironment SchemaTestT>"

  local :: (SchemaEnvironment -> SchemaEnvironment) -> SchemaTestT a -> SchemaTestT a
  local = notImplemented "local<MonadReader SchemaEnvironment SchemaTestT>"

-------------------------------------------------------------------------------

-- | ParserTestT
--
-- Encodes an assertion error (as `Left`) or a value as `Right`.
newtype ParserTestT a = ParserTestT (Either (IO ()) a)
  deriving stock (Functor)
  deriving (Applicative, Monad) via (Either (IO ()))

instance MonadSchema ParserTestT SchemaTestT where
  memoizeOn :: TH.Name -> a -> SchemaTestT (p ParserTestT b) -> SchemaTestT (p ParserTestT b)
  memoizeOn _ _ = id

instance MonadParse ParserTestT where
  withPath :: (JSONPath -> JSONPath) -> ParserTestT a -> ParserTestT a
  withPath = const id

  parseErrorWith :: Code -> Text -> ParserTestT a
  parseErrorWith code text =
    ParserTestT
      . Left
      . expectationFailure
      $ show code <> ": " <> T.unpack text
