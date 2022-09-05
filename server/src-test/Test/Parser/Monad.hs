{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | This module defines the monads required to run parser tests.
--
-- Warning: a lot of the implementations are currently 'undefined'. As we write
-- more advanced tests, they might require implementations.
module Test.Parser.Monad
  ( ParserTestM (..),
    SchemaEnvironment,
    SchemaTestM (..),
    notImplementedYet,
  )
where

import Control.Monad.Memoize
import Data.Aeson.Internal (JSONPathElement)
import Data.Has (Has (..))
import Data.Text qualified as T
import GHC.Stack
import Hasura.Base.Error (QErr)
import Hasura.Base.ErrorMessage
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Parser.ErrorCode
import Hasura.GraphQL.Schema.Common (SchemaContext (..), SchemaKind (..), ignoreRemoteRelationship)
import Hasura.GraphQL.Schema.NamingCase
import Hasura.GraphQL.Schema.Options (SchemaOptions (..))
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.GraphQL.Schema.Typename
import Hasura.Prelude
import Hasura.RQL.Types.SourceCustomization (CustomizeRemoteFieldName, MkRootFieldName)
import Hasura.Session (adminRoleName)
import Language.Haskell.TH.Syntax qualified as TH
import Test.HUnit.Lang (assertFailure)

-- | Placeholder value for test inputs that are not relevant yet.
notImplementedYet :: HasCallStack => String -> a
notImplementedYet thing =
  error $
    ( unlines
        [ "\"" ++ thing ++ "\" is not yet defined, because it hasn't been touched by tests yet.",
          "If you see this message you likely need to provide/mock a value here"
        ]
    )

-- | Monad builder environment.
--
-- Parser functions generally have a return type of @m (Parser n)@. The @m@
-- parameter is mocked through 'SchemaTestM', which requires a bunch of 'Has'
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
  modifier = notImplementedYet "modifier<Has NamingCase SchemaEnvironment>"

instance Has SchemaOptions SchemaEnvironment where
  getter :: SchemaEnvironment -> SchemaOptions
  getter =
    const
      SchemaOptions
        { soStringifyNumbers = Options.Don'tStringifyNumbers,
          soDangerousBooleanCollapse = Options.Don'tDangerouslyCollapseBooleans,
          soInferFunctionPermissions = Options.InferFunctionPermissions,
          soOptimizePermissionFilters = Options.Don'tOptimizePermissionFilters
        }

  modifier :: (SchemaOptions -> SchemaOptions) -> SchemaEnvironment -> SchemaEnvironment
  modifier = notImplementedYet "modifier<Has SchemaOptions SchemaEnvironment>"

instance Has SchemaContext SchemaEnvironment where
  getter :: SchemaEnvironment -> SchemaContext
  getter =
    const
      SchemaContext
        { scSchemaKind = HasuraSchema,
          scRemoteRelationshipParserBuilder = ignoreRemoteRelationship,
          scRole = adminRoleName
        }

  modifier :: (SchemaContext -> SchemaContext) -> SchemaEnvironment -> SchemaEnvironment
  modifier = notImplementedYet "modifier<Has SchemaContext SchemaEnvironment>"

instance Has MkTypename SchemaEnvironment where
  getter :: SchemaEnvironment -> MkTypename
  getter = const (MkTypename id)

  modifier :: (MkTypename -> MkTypename) -> SchemaEnvironment -> SchemaEnvironment
  modifier = notImplementedYet "modifier<Has MkTypeName SchemaEnvironment>"

instance Has MkRootFieldName SchemaEnvironment where
  getter :: SchemaEnvironment -> MkRootFieldName
  getter = const mempty

  modifier :: (MkRootFieldName -> MkRootFieldName) -> SchemaEnvironment -> SchemaEnvironment
  modifier = notImplementedYet "modifier<Has MkRootFieldName SchemaEnvironment>"

instance Has CustomizeRemoteFieldName SchemaEnvironment where
  getter :: SchemaEnvironment -> CustomizeRemoteFieldName
  getter = notImplementedYet "getter<Has CustomizeRemoteFieldName SchemaEnvironment>"

  modifier :: (CustomizeRemoteFieldName -> CustomizeRemoteFieldName) -> SchemaEnvironment -> SchemaEnvironment
  modifier = notImplementedYet "modifier<Has CustomizeRemoteFieldName SchemaEnvironment>"

-------------------------------------------------------------------------------

-- | SchemaTestM
newtype SchemaTestM a = SchemaTestM {runSchemaTestM :: a}
  deriving stock (Functor)
  deriving (Applicative, Monad) via Identity

instance MonadError QErr SchemaTestM where
  throwError :: forall a. QErr -> SchemaTestM a
  throwError = notImplementedYet "throwError<MonadError QErr SchemaTestM>"

  catchError :: forall a. SchemaTestM a -> (QErr -> SchemaTestM a) -> SchemaTestM a
  catchError = notImplementedYet "catchError<MonadError QErr SchemaTestM>"

-- | Note this is not used because all the actual getters/setters for
-- SchemaEnvironment are @const X@, so these bottoms never actually get
-- evaluated.
instance MonadReader SchemaEnvironment SchemaTestM where
  ask :: SchemaTestM SchemaEnvironment
  ask = notImplementedYet "ask<MonadReader SchemaEnvironment SchemaTestM>"

  local :: (SchemaEnvironment -> SchemaEnvironment) -> SchemaTestM a -> SchemaTestM a
  local = notImplementedYet "local<MonadReader SchemaEnvironment SchemaTestM>"

-------------------------------------------------------------------------------

-- | ParserTestM
--
-- Encodes an assertion error (as `Left`) or a value as `Right`.
-- newtype ParserTestM a = ParserTestM {runParserTestM :: Either (IO ()) a}
newtype ParserTestM a = ParserTestM {runParserTestM :: IO a}
  deriving stock (Functor)
  -- deriving (Applicative, Monad) via (Either (IO ()))
  deriving newtype (Applicative, Monad)

instance MonadMemoize SchemaTestM where
  memoizeOn :: TH.Name -> a -> SchemaTestM p -> SchemaTestM p
  memoizeOn _ _ = id

instance MonadParse ParserTestM where
  withKey :: JSONPathElement -> ParserTestM a -> ParserTestM a
  withKey = const id

  parseErrorWith :: ParseErrorCode -> ErrorMessage -> ParserTestM a
  parseErrorWith code text =
    ParserTestM . assertFailure $ show code <> ": " <> T.unpack (fromErrorMessage text)
