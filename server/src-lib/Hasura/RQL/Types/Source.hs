{-# LANGUAGE AllowAmbiguousTypes #-}

module Hasura.RQL.Types.Source where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                 as M

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Typeable                       (cast)

import qualified Hasura.Tracing                      as Tracing

import           Hasura.Backends.Postgres.Connection
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Function
import           Hasura.RQL.Types.Table
import           Hasura.SQL.Backend
import           Hasura.Session


{- Note [Existentially Quantified Types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This note contains a brief introduction to existential types, along with some
examples from this codebase on how to deal with such types.

If we consider the identity function:

    id :: forall a. a -> a

Then for all /callers/ of this function, the type variable 'a' is universally
quantified: the caller can pick any type for 'a' when calling the function.

On the other hand, the /implementer/ of this function cannot pick an 'a'. From
this perspective, the type variable 'a' is existentially quantified.

Let's consider a rank-2 function:

    rank2 :: forall a. (forall b. b -> String) -> a -> String

In this example, the /caller/ gets to pick 'a' since it's universally quantified,
but 'b' is existentially quantified from this perspective. We have to provide
a function that works for any 'b' the implementer may pick!

From the perspective of the /implementer/, 'a' is existentially quantified,
whereas 'b' is universally quantified: we (the implementers) get to pick
'b' (and we may call it multiple times with different types!).


One thing that we cannot do is we cannot return an existentially quantified
value. In order to do that, we need to wrap it in a constructor, e.g.:

    data Exists = forall a. Exists a

Normally, type variables that appear on the right hand side of a type declaration
also appear on the left hand side. This is precisely what existential quantification
relaxes.

IMPORTANT: please keep in mind that existential types /erase/ all type information.

Similarly to implementing the 'id' function), there are few functions we can write
without more context:

    idExists :: Exists -> Exists
    idExists (Exists a) = Exists a

    existsList :: Exists
    existsList = [ Exists "hello", Exists (Just '1'), Exists (42 :: Int) ]


However, we can't do anything else: we cannot recover the original values or do
any operations. The way to deal with this problem is to pack a dictionary along
with the value. The most common example is 'Showable':

    data Showable = forall a. Show a => Showable a

    showShowable :: Showable -> String
    showShowable (Showable a) = show a

We are able to call 'show' on 'a' because we are /packing/ the 'Show' constraint
along with the value. This is key to using existential types.

For details on how we use existentials in our code, please see note
[Recovering Existentially Quantified Type Information] -}

data SourceInfo b
  = SourceInfo
  { _siName          :: !SourceName
  , _siTables        :: !(TableCache b)
  , _siFunctions     :: !(FunctionCache b)
  , _siConfiguration :: !(SourceConfig b)
  } deriving (Generic)
$(makeLenses ''SourceInfo)
instance Backend b => ToJSON (SourceInfo b) where
  toJSON = genericToJSON hasuraJSON

-- See Note [Existentially Quantified Types]
data BackendSourceInfo =
  forall b. Backend b => BackendSourceInfo (SourceInfo b)

instance ToJSON BackendSourceInfo where
  toJSON (BackendSourceInfo si) = toJSON si

type SourceCache = HashMap SourceName BackendSourceInfo

-- Those functions cast the content of BackendSourceInfo in order to extract
-- a backend-specific SourceInfo. Ideally, those functions should NOT be used:
-- the rest of the code should be able to deal with any source, regardless of
-- backend, through usage of the appropriate typeclasses.
-- They are thus a temporary workaround as we work on generalizing code that
-- uses the schema cache.

unsafeSourceInfo :: forall b. Backend b => BackendSourceInfo -> Maybe (SourceInfo b)
unsafeSourceInfo (BackendSourceInfo si) = cast si

unsafeSourceName :: BackendSourceInfo -> SourceName
unsafeSourceName (BackendSourceInfo (SourceInfo name _ _ _)) = name

unsafeSourceTables :: forall b. Backend b => BackendSourceInfo -> Maybe (TableCache b)
unsafeSourceTables = fmap _siTables . unsafeSourceInfo @b

unsafeSourceFunctions :: forall b. Backend b => BackendSourceInfo -> Maybe (FunctionCache b)
unsafeSourceFunctions = fmap _siFunctions . unsafeSourceInfo @b

unsafeSourceConfiguration :: forall b. Backend b => BackendSourceInfo -> Maybe (SourceConfig b)
unsafeSourceConfiguration = fmap _siConfiguration . unsafeSourceInfo @b


getTableRoles :: BackendSourceInfo -> [RoleName]
getTableRoles (BackendSourceInfo si) = M.keys . _tiRolePermInfoMap =<< M.elems (_siTables si)


-- | Contains Postgres connection configuration and essential metadata from the
-- database to build schema cache for tables and function.
data ResolvedSource b
  = ResolvedSource
  { _rsConfig    :: !(SourceConfig b)
  , _rsTables    :: !(DBTablesMetadata b)
  , _rsFunctions :: !(DBFunctionsMetadata b)
  , _rsPgScalars :: !(HashSet (ScalarType b))
  } deriving (Eq)

type SourceTables b = HashMap SourceName (TableCache b)

type SourceResolver =
  SourceName -> PostgresConnConfiguration -> IO (Either QErr (SourceConfig 'Postgres))

class (Monad m) => MonadResolveSource m where
  getSourceResolver :: m SourceResolver

instance (MonadResolveSource m) => MonadResolveSource (ExceptT e m) where
  getSourceResolver = lift getSourceResolver

instance (MonadResolveSource m) => MonadResolveSource (ReaderT r m) where
  getSourceResolver = lift getSourceResolver

instance (MonadResolveSource m) => MonadResolveSource (Tracing.TraceT m) where
  getSourceResolver = lift getSourceResolver

instance (MonadResolveSource m) => MonadResolveSource (LazyTxT QErr m) where
  getSourceResolver = lift getSourceResolver

-- Metadata API related types
data AddSource b
  = AddSource
  { _asName          :: !SourceName
  , _asConfiguration :: !(SourceConnConfiguration b)
  } deriving (Generic)
deriving instance (Backend b) => Show (AddSource b)
deriving instance (Backend b) => Eq (AddSource b)

instance (Backend b) => ToJSON (AddSource b) where
  toJSON = genericToJSON hasuraJSON

instance (Backend b) => FromJSON (AddSource b) where
  parseJSON = genericParseJSON hasuraJSON

data DropSource
  = DropSource
  { _dsName    :: !SourceName
  , _dsCascade :: !Bool
  } deriving (Show, Eq)
$(deriveToJSON hasuraJSON ''DropSource)

instance FromJSON DropSource where
  parseJSON = withObject "Object" $ \o ->
    DropSource <$> o .: "name" <*> o .:? "cascade" .!= False

newtype PostgresSourceName =
  PostgresSourceName {_psnName :: SourceName}
  deriving (Show, Eq)
$(deriveJSON hasuraJSON ''PostgresSourceName)
