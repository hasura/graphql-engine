{-# LANGUAGE StrictData #-}

module Hasura.GraphQL.Parser.Column
  ( UnpreparedValue(..)

  , Opaque
  , mkOpaque
  , openOpaque
  , mkParameter
  ) where

import           Hasura.Prelude

import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Schema
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Column      hiding (EnumValue (..), EnumValueInfo (..))
import           Hasura.SQL.Backend
import           Hasura.Session               (SessionVariable)

-- -------------------------------------------------------------------------------------------------

data Opaque a = Opaque
  { _opVariable :: Maybe VariableInfo
  -- ^ The variable this value came from, if any.
  , _opValue    :: a
  } -- Note: we intentionally don’t derive any instances here, since that would
    -- defeat the opaqueness!

mkOpaque :: Maybe VariableInfo -> a -> Opaque a
mkOpaque = Opaque

openOpaque :: MonadParse m => Opaque a -> m a
openOpaque (Opaque Nothing  value) = pure value
openOpaque (Opaque (Just _) value) = markNotReusable $> value

data UnpreparedValue (b :: BackendType)
  -- | A SQL value that can be parameterized over.
  = UVParameter
      (Maybe VariableInfo)
      -- ^ The GraphQL variable this value came from, if any.
      (ColumnValue b)
  -- | A literal SQL expression that /cannot/ be parameterized over.
  | UVLiteral (SQLExpression b)
  -- | The entire session variables JSON object.
  | UVSession
  -- | A single session variable.
  | UVSessionVar (SessionVarType b) SessionVariable

-- FIXME exporting this method means doing away with the opaqueness of the
-- 'Opaque' data type, since the constructors of 'UnpreparedValue' are exported
-- globally.
mkParameter :: Opaque (ColumnValue b) -> UnpreparedValue b
mkParameter (Opaque variable value) = UVParameter variable value
