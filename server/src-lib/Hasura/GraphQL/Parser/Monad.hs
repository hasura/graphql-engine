{-# LANGUAGE StrictData #-}

-- | Monad transformers for GraphQL schema construction and query parsing.
module Hasura.GraphQL.Parser.Monad
  ( SchemaT
  , runSchemaT

  , ParseT
  , ParseError(..)
  ) where

import           Hasura.Prelude

import qualified Data.Dependent.Map                    as DM
import qualified Language.Haskell.TH                   as TH

import           Control.Monad.Primitive               (PrimMonad, stToPrim)
import           Control.Monad.ST.Unsafe               (unsafeInterleaveST)
import           Control.Monad.Unique
import           Control.Monad.Validate
import           Data.Dependent.Map                    (DMap)
import           Data.GADT.Compare.Extended
import           Data.Parser.JSONPath
import           Data.Primitive.MutVar
import           Data.Proxy                            (Proxy (..))
import           Type.Reflection                       (Typeable, typeRep)

import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Internal.Parser
import           Hasura.GraphQL.Parser.Schema

-- -------------------------------------------------------------------------------------------------
-- schema construction

newtype SchemaT n m a = SchemaT
  { unSchemaT :: StateT (DMap ParserId (ParserById n)) m a
  } deriving (Functor, Applicative, Monad, MonadTrans)

runSchemaT :: Monad m => SchemaT n m a -> m a
runSchemaT = flip evalStateT mempty . unSchemaT

instance (PrimMonad m, MonadUnique m, MonadParse n) => MonadSchema n (SchemaT n m) where
  memoize name buildParser arg = SchemaT do
    let parserId = ParserId name arg
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
        cell <- newMutVar Nothing

        -- We use unsafeInterleaveST here, which sounds scary, but
        -- unsafeInterleaveIO/ST is actually far more safe than unsafePerformIO.
        -- unsafeInterleave just defers the execution of the action until its
        -- result is needed, adding some laziness.
        --
        -- That laziness can be dangerous if the action has side-effects, since
        -- the point at which the effect is performed can be unpredictable. But
        -- this action just reads, never writes, so that isn’t a concern.
        parserById <- stToPrim $ unsafeInterleaveST $ readMutVar cell >>= \case
          Just parser -> pure $ ParserById parser
          Nothing -> error $ unlines
            [ "memoize: parser was forced before being fully constructed"
            , "  parser constructor: " ++ TH.pprint name ]
        put $! DM.insert parserId parserById parsersById

        unique <- newUnique
        parser <- addDefinitionUnique unique <$> unSchemaT (buildParser arg)
        writeMutVar cell (Just parser)
        pure parser

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
  { unParseT :: ReaderT JSONPath (StateT QueryReusability (ValidateT ParseError m)) a
  } deriving (Functor, Applicative, Monad)

instance MonadTrans ParseT where
  lift = ParseT . lift . lift . lift

data ParseError = ParseError
  { pePath    :: JSONPath
  , peMessage :: Text }
