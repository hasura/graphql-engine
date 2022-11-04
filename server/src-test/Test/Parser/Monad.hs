-- | This module defines the monads required to run parser tests.
--
-- Warning: a lot of the implementations are currently 'undefined'. As we write
-- more advanced tests, they might require implementations.
module Test.Parser.Monad
  ( ParserTest (..),
    SchemaEnvironment (..),
    SchemaTest,
    runSchemaTest,
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
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.NamingCase
import Hasura.GraphQL.Schema.Options (SchemaOptions (..))
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.GraphQL.Schema.Typename
import Hasura.Prelude
import Hasura.RQL.Types.SourceCustomization (MkRootFieldName)
import Hasura.RemoteSchema.SchemaCache (CustomizeRemoteFieldName)
import Hasura.Session (adminRoleName)
import Language.Haskell.TH.Syntax qualified as TH
import Test.HUnit.Lang (assertFailure)

-- | Placeholder value for test inputs that are not relevant yet.
notImplementedYet :: HasCallStack => String -> a
notImplementedYet thing =
  withFrozenCallStack $
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
data SchemaEnvironment = SchemaEnvironment
  {seSchemaOptions :: SchemaOptions}

defaultSchemaOptions :: SchemaOptions
defaultSchemaOptions =
  SchemaOptions
    { soStringifyNumbers = Options.Don'tStringifyNumbers,
      soDangerousBooleanCollapse = Options.Don'tDangerouslyCollapseBooleans,
      soInferFunctionPermissions = Options.InferFunctionPermissions,
      soOptimizePermissionFilters = Options.Don'tOptimizePermissionFilters,
      soIncludeUpdateManyFields = Options.IncludeUpdateManyFields,
      soIncludeAggregationPredicates = Options.IncludeAggregationPredicates,
      soIncludeStreamFields = Options.IncludeStreamFields,
      soBigQueryStringNumericInput = Options.EnableBigQueryStringNumericInput
    }

instance Has NamingCase SchemaEnvironment where
  getter :: SchemaEnvironment -> NamingCase
  getter = const HasuraCase

  modifier :: (NamingCase -> NamingCase) -> SchemaEnvironment -> SchemaEnvironment
  modifier = notImplementedYet "modifier<Has NamingCase SchemaEnvironment>"

instance Has SchemaOptions SchemaEnvironment where
  getter :: SchemaEnvironment -> SchemaOptions
  getter =
    seSchemaOptions

  modifier :: (SchemaOptions -> SchemaOptions) -> SchemaEnvironment -> SchemaEnvironment
  modifier f env = env {seSchemaOptions = f (seSchemaOptions env)}

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

-- | SchemaTest
type SchemaTest = SchemaT SchemaEnvironment SchemaTestInternal

runSchemaTest :: SchemaTest a -> a
runSchemaTest = runSchemaTestInternal . flip runReaderT (SchemaEnvironment defaultSchemaOptions) . runSchemaT

newtype SchemaTestInternal a = SchemaTestInternal {runSchemaTestInternal :: a}
  deriving stock (Functor)
  deriving (Applicative, Monad) via Identity

instance MonadError QErr SchemaTestInternal where
  throwError :: forall a. QErr -> SchemaTestInternal a
  throwError = notImplementedYet "throwError<MonadError QErr SchemaTestT>"

  catchError :: forall a. SchemaTestInternal a -> (QErr -> SchemaTestInternal a) -> SchemaTestInternal a
  catchError = notImplementedYet "catchError<MonadError QErr SchemaTestInternal>"

instance MonadMemoize SchemaTestInternal where
  memoizeOn :: TH.Name -> a -> SchemaTestInternal p -> SchemaTestInternal p
  memoizeOn _ _ = id

-------------------------------------------------------------------------------

-- | ParserTest
newtype ParserTest a = ParserTest {runParserTest :: IO a}
  deriving stock (Functor)
  deriving newtype (Applicative, Monad)

instance MonadParse ParserTest where
  withKey :: JSONPathElement -> ParserTest a -> ParserTest a
  withKey = const id

  parseErrorWith :: ParseErrorCode -> ErrorMessage -> ParserTest a
  parseErrorWith code text =
    ParserTest . assertFailure $ show code <> ": " <> T.unpack (fromErrorMessage text)
