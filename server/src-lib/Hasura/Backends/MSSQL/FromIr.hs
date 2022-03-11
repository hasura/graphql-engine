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
    runFromIr,
    Error (..),

    -- * Name generation
    NameTemplate (..),
    generateAlias,
  )
where

import Control.Monad.Validate
import Control.Monad.Validate qualified as V
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Hasura.Backends.MSSQL.Instances.Types ()
import Hasura.Backends.MSSQL.Types.Internal as TSQL
import Hasura.Base.Error (QErr, throw500)
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.SQL.Backend

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
  { unFromIr :: StateT (Map Text Int) (Validate (NonEmpty Error)) a
  }
  deriving (Functor, Applicative, Monad, MonadValidate (NonEmpty Error))

-- | Run a 'FromIr' action, throwing errors that have been collected using the
-- supplied action.
runFromIr :: MonadError QErr m => FromIr a -> m a
runFromIr = flip onLeft (throw500 . tshow) . V.runValidate . flip evalStateT mempty . unFromIr

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
