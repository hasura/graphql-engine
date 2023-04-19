-- | The modules in the @Hasura.Backends.MSSQL.FromIr@ namespace translates the
-- RQL IR into TSQL, the SQL dialect of MSSQL, as defined in abstract syntax in
-- "Hasura.Backends.MSSQL.Types".
--
-- The translation happens in the @FromIr@ monad, which manages identifier
-- scoping and error collection.
--
-- The actual rendering of this AST into TSQL text happens in
-- "Hasura.Backends.MSSQL.ToQuery".
module Hasura.Backends.MSSQL.FromIr
  ( -- * The central Monad
    FromIr,
    runFromIrDiscardCTEs,
    runFromIrUseCTEs,
    runFromIrUseCTEsT,
    Error (..),
    tellBefore,
    tellAfter,
    tellCTE,

    -- * Name generation
    NameTemplate (..),
    generateAlias,
  )
where

import Control.Monad.Validate
import Control.Monad.Validate qualified as V
import Control.Monad.Writer.Strict
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Hasura.Backends.MSSQL.Instances.Types ()
import Hasura.Backends.MSSQL.Types.Internal as TSQL
import Hasura.Base.Error (QErr, throw500)
import Hasura.NativeQuery.Metadata (InterpolatedQuery)
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.SQL.Backend

-- | Allow the query process to emit extra setup / teardown steps
data IRWriter = IRWriter
  { irwBefore :: [TempTableDDL],
    irwAfter :: [TempTableDDL],
    irwCTEs :: Maybe With
  }

instance Semigroup IRWriter where
  (IRWriter a b c) <> (IRWriter a' b' c') = IRWriter (a <> a') (b' <> b) (c <> c')

instance Monoid IRWriter where
  mempty = IRWriter mempty mempty Nothing

-- | add a step to be run before the main query
tellBefore :: TempTableDDL -> FromIr ()
tellBefore step =
  tell (IRWriter {irwBefore = [step], irwAfter = mempty, irwCTEs = Nothing})

-- | add a step to be run after the main query
tellAfter :: TempTableDDL -> FromIr ()
tellAfter step =
  tell (IRWriter {irwBefore = mempty, irwAfter = [step], irwCTEs = Nothing})

tellCTE :: Aliased (InterpolatedQuery Expression) -> FromIr ()
tellCTE cte =
  tell (IRWriter {irwBefore = mempty, irwAfter = mempty, irwCTEs = Just (With $ pure $ CTEUnsafeRawSQL <$> cte)})

-- | The central Monad used throughout for all conversion functions.
--
-- It has the following features:
--
-- * It's a 'MonadValidate', so it'll continue going when it encounters 'Error's
--   to accumulate as many as possible.
--
-- * It has a facility for generating fresh, unique aliases, which lets the
--   translation output retain a resemblance with source names without the
--   translation process needing to be bothered about potential name shadowing.
--   See 'generateAlias'.
newtype FromIr a = FromIr
  { unFromIr ::
      WriterT
        IRWriter
        ( StateT
            (Map Text Int)
            (Validate (NonEmpty Error))
        )
        a
  }
  deriving (Functor, Applicative, Monad, MonadValidate (NonEmpty Error), MonadWriter IRWriter)

-- | Run a 'FromIr' action, throwing errors that have been collected using the
-- supplied action.
runFromIr ::
  (Traversable t, MonadError QErr m) =>
  ((a, IRWriter) -> QueryWithDDL a) ->
  t (FromIr a) ->
  m (t (QueryWithDDL a))
runFromIr toQueryWithDDL =
  flip onLeft (throw500 . tshow)
    . V.runValidate
    . flip evalStateT mempty
    . fmap (fmap toQueryWithDDL)
    . traverse (runWriterT . unFromIr)

-- | Run a 'FromIr' action, throwing errors that have been collected using the
-- supplied action, and attach CTEs created from native queries to the select query.
runFromIrUseCTEs :: MonadError QErr m => FromIr Select -> m (QueryWithDDL Select)
runFromIrUseCTEs fromir = runIdentity <$> runFromIr attachCTEs (Identity fromir)

-- | Run a 'FromIr' action, throwing errors that have been collected using the
-- supplied action, and discard CTEs created from native queries to the select query.
runFromIrDiscardCTEs :: MonadError QErr m => FromIr a -> m (QueryWithDDL a)
runFromIrDiscardCTEs fromir = runIdentity <$> runFromIr discardCTEs (Identity fromir)

-- | Run a 'FromIr' action, throwing errors that have been collected using the
-- supplied action, and attach CTEs created from native queries to the select query.
runFromIrUseCTEsT :: (Traversable t, MonadError QErr m) => t (FromIr Select) -> m (t (QueryWithDDL Select))
runFromIrUseCTEsT = runFromIr attachCTEs

attachCTEs :: (Select, IRWriter) -> QueryWithDDL Select
attachCTEs (select, IRWriter before after ctes) =
  QueryWithDDL before select {selectWith = ctes <> selectWith select} after

discardCTEs :: (a, IRWriter) -> QueryWithDDL a
discardCTEs (a, IRWriter before after _ctes) =
  -- TODO: assert ctes is empty, or throw an error "not supported"
  QueryWithDDL before a after

-- | Errors that may happen during translation.
data Error
  = UnsupportedOpExpG (IR.OpExpG 'MSSQL Expression)
  | FunctionNotSupported
  deriving (Show, Eq)

-- | Hints about the type of entity that 'generateAlias' is producing an alias
-- for.
data NameTemplate
  = ArrayRelationTemplate Text
  | ArrayAggregateTemplate Text
  | ObjectRelationTemplate Text
  | TableTemplate Text
  | ForOrderAlias Text

-- | Generate a fresh alias for a given entity to remove ambiguity and naming
-- conflicts between scopes at the TSQL level.
--
-- Names are generated in the form @type_name_occurrence@, where:
--
--  * @type@ hints at the type of entity,
--  * @name@ refers to the source name being aliased, and
--  * @occurrence@ is an integer counter that distinguishes each occurrence of @type_name@.
--
-- Example outputs:
--
-- > do
-- >   "ar_articles_1" <- generateAlias (ArrayRelationTemplate "articles")
-- >   "ar_articles_2" <- generateAlias (ArrayRelationTemplate "articles")
-- >   "t_users_1"     <- generateAlias (TableTemplate "users")
generateAlias :: NameTemplate -> FromIr Text
generateAlias template = do
  FromIr (modify' (M.insertWith (+) rendered 1))
  occurrence <- M.findWithDefault 1 rendered <$> FromIr get
  pure (rendered <> tshow occurrence)
  where
    rendered = T.take 20 $
      case template of
        ArrayRelationTemplate sample -> "ar_" <> sample
        ArrayAggregateTemplate sample -> "aa_" <> sample
        ObjectRelationTemplate sample -> "or_" <> sample
        TableTemplate sample -> "t_" <> sample
        ForOrderAlias sample -> "order_" <> sample
