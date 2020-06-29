{-# LANGUAGE StrictData #-}

-- | Monad transformers for GraphQL schema construction and query parsing.
module Hasura.GraphQL.Parser.Monad
  ( SchemaT
  , runSchemaT

  , ParseT
  , runParseT
  , ParseError(..)
  ) where

import           Hasura.Prelude

import qualified Data.Dependent.Map                    as DM
import qualified Data.HashMap.Strict                   as M
import qualified Data.Sequence.NonEmpty                as NE
import qualified Language.Haskell.TH                   as TH

import           Control.Monad.Unique
import           Control.Monad.Validate
import           Data.Dependent.Map                    (DMap)
import           Data.GADT.Compare.Extended
import           Data.IORef
import           Data.Parser.JSONPath
import           Data.Proxy                            (Proxy (..))
import           System.IO.Unsafe                      (unsafeInterleaveIO)
import           Type.Reflection                       ((:~:)(..), Typeable, typeRep)

import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Internal.Parser
import           Hasura.GraphQL.Parser.Schema
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Permission           (RoleName)
import           Hasura.RQL.Types.Table                (TableCache)
import           Hasura.SQL.Types

-- -------------------------------------------------------------------------------------------------
-- schema construction

newtype SchemaT n m a = SchemaT
  { unSchemaT :: ReaderT (RoleName, TableCache) (StateT (DMap ParserId (ParserById n)) m) a
  } deriving (Functor, Applicative, Monad, MonadError e)

instance Monad m => MonadReader a (SchemaT n (ReaderT a m)) where
  ask = SchemaT $ lift $ lift ask
  local f (SchemaT x) = SchemaT $ mapReaderT (mapStateT (local f)) x
  reader f = SchemaT $ lift $ lift $ reader f

runSchemaT :: Monad m => RoleName -> TableCache -> SchemaT n m a -> m a
runSchemaT roleName tableCache = unSchemaT
  >>> flip runReaderT (roleName, tableCache)
  >>> flip evalStateT mempty

-- | see Note [SchemaT requires MonadIO]
instance (MonadIO m, MonadUnique m, MonadError QErr m, MonadParse n)
      => MonadSchema n (SchemaT n m) where
  askRoleName = SchemaT $ asks fst

  askTableInfo tableName = SchemaT $ do
    tableInfo <- asks $ M.lookup tableName . snd
    -- This should never fail, since the schema cache construction process is
    -- supposed to ensure that all dependencies are resolved.
    tableInfo `onNothing` throw500 ("SchemaT.askTableInfo: no info for " <>> tableName)

  memoizeOn name key buildParser = SchemaT do
    let parserId = ParserId name key
    parsersById <- get
    case DM.lookup parserId parsersById of
      Just (ParserById parser) -> pure parser
      Nothing -> do
        -- We manually do eager blackholing here using a MutVar rather than
        -- relying on MonadFix and ordinary thunk blackholing. Why? A few
        -- reasons:
        --
        --   1. We have more control. We aren’t at the whims of whatever
        --      MonadFix instance happens to get used.
        --
        --   2. We can be more precise. GHC’s lazy blackholing doesn’t always
        --      kick in when you’d expect.
        --
        --   3. We can provide more useful error reporting if things go wrong.
        --      Most usefully, we can include a HasCallStack source location.
        cell <- liftIO $ newIORef Nothing

        -- We use unsafeInterleaveIO here, which sounds scary, but
        -- unsafeInterleaveIO is actually far more safe than unsafePerformIO.
        -- unsafeInterleaveIO just defers the execution of the action until its
        -- result is needed, adding some laziness.
        --
        -- That laziness can be dangerous if the action has side-effects, since
        -- the point at which the effect is performed can be unpredictable. But
        -- this action just reads, never writes, so that isn’t a concern.
        parserById <- liftIO $ unsafeInterleaveIO $ readIORef cell >>= \case
          Just parser -> pure $ ParserById parser
          Nothing -> error $ unlines
            [ "memoize: parser was forced before being fully constructed"
            , "  parser constructor: " ++ TH.pprint name ]
        put $! DM.insert parserId parserById parsersById

        unique <- newUnique
        parser <- addDefinitionUnique unique <$> unSchemaT buildParser
        liftIO $ writeIORef cell (Just parser)
        pure parser

{- Note [SchemaT requires MonadIO]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The MonadSchema instance for SchemaT requires MonadIO, which is unsatisfying.
The only reason the constraint is needed is to implement knot-tying via IORefs
(see Note [Tying the knot] in Hasura.GraphQL.Parser.Class), which really only
requires the power of ST. Using ST would be much nicer, since we could discharge
the burden locally, but unfortunately we also want to use MonadUnique, which
is handled by IO in the end.

This means that we need IO at the base of our monad, so to use STRefs, we’d need
a hypothetical STT transformer (i.e. a monad transformer version of ST). But
such a thing isn’t safe in general, since reentrant monads like ListT or ContT
would incorrectly share state between the different threads of execution.

In theory, this can be resolved by using something like Vault (from the vault
package) to create “splittable” sets of variable references. That would allow
you to create a transformer with an STRef-like interface that works over any
arbitrary monad. However, while the interface would be safe, the implementation
of such an abstraction requires unsafe primitives, and to the best of my
knowledge no such transformer exists in any existing libraries.

So we decide it isn’t worth the trouble and just use MonadIO. If `eff` ever pans
out, it should be able to support this more naturally, so we can fix it then. -}

-- | A key used to distinguish calls to 'memoize'd functions. The 'TH.Name'
-- distinguishes calls to completely different parsers, and the @a@ value
-- records the arguments.
data ParserId (t :: (Kind, *)) where
  ParserId :: (Ord a, Typeable a, Typeable b, Typeable k) => TH.Name -> a -> ParserId '(k, b)

instance GEq ParserId where
  geq (ParserId name1 (arg1 :: a1) :: ParserId t1)
      (ParserId name2 (arg2 :: a2) :: ParserId t2)
    | _ :: Proxy '(k1, b1) <- Proxy @t1
    , _ :: Proxy '(k2, b2) <- Proxy @t2
    , name1 == name2
    , Just Refl <- typeRep @a1 `geq` typeRep @a2
    , arg1 == arg2
    , Just Refl <- typeRep @k1 `geq` typeRep @k2
    , Just Refl <- typeRep @b1 `geq` typeRep @b2
    = Just Refl
    | otherwise = Nothing

instance GCompare ParserId where
  gcompare (ParserId name1 (arg1 :: a1) :: ParserId t1)
           (ParserId name2 (arg2 :: a2) :: ParserId t2)
    | _ :: Proxy '(k1, b1) <- Proxy @t1
    , _ :: Proxy '(k2, b2) <- Proxy @t2
    = strengthenOrdering (compare name1 name2)
      `extendGOrdering` gcompare (typeRep @a1) (typeRep @a2)
      `extendGOrdering` strengthenOrdering (compare arg1 arg2)
      `extendGOrdering` gcompare (typeRep @k1) (typeRep @k2)
      `extendGOrdering` gcompare (typeRep @b1) (typeRep @b2)
      `extendGOrdering` GEQ

-- | A newtype wrapper around a 'Parser' that rearranges the type parameters
-- so that it can be indexed by a 'ParserId' in a 'DMap'.
--
-- This is really just a single newtype, but it’s implemented as a data family
-- because GHC doesn’t allow ordinary datatype declarations to pattern-match on
-- type parameters, and we want to match on the tuple.
data family ParserById (m :: * -> *) (a :: (Kind, *))
newtype instance ParserById m '(k, a) = ParserById (Parser k m a)

-- -------------------------------------------------------------------------------------------------
-- query parsing

newtype ParseT m a = ParseT
  { unParseT :: ReaderT JSONPath (StateT QueryReusability (ValidateT (NESeq ParseError) m)) a
  } deriving (Functor, Applicative, Monad)

runParseT
  :: Functor m
  => ParseT m a
  -> m (Either (NESeq ParseError) (a, QueryReusability))
runParseT = unParseT
  >>> flip runReaderT []
  >>> flip runStateT mempty
  >>> runValidateT

instance MonadTrans ParseT where
  lift = ParseT . lift . lift . lift

instance Monad m => MonadParse (ParseT m) where
  withPath f x = ParseT $ withReaderT f $ unParseT x
  parseError text = ParseT $ do
    path <- ask
    lift $ refute $ NE.singleton ParseError{ pePath = path, peMessage = text }
  markNotReusable = ParseT $ lift $ put NotReusable

data ParseError = ParseError
  { pePath    :: JSONPath
  , peMessage :: Text }
