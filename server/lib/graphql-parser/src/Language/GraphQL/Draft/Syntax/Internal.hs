{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | Internal GraphQL AST functionality.
--
-- This module is primarily necessary due to an incorrect
-- @-Wredundant-constraints@ warning emitted by GHC when compiling
-- 'liftTypedHashMap'.
module Language.GraphQL.Draft.Syntax.Internal
  ( liftTypedHashMap,
  )
where

-------------------------------------------------------------------------------

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Language.Haskell.TH.Syntax (Lift, liftTyped)
import Language.Haskell.TH.Syntax qualified as TH
import Prelude

-------------------------------------------------------------------------------

-- | Lift a 'HashMap' into a Template Haskell splice via list conversion.
#if MIN_VERSION_template_haskell(2,17,0)
liftTypedHashMap ::
  ( Eq k,
    Hashable k,
    Lift k,
    Lift v,
    TH.Quote m
  ) =>
  HashMap k v ->
  TH.Code m (HashMap k v)
#else
liftTypedHashMap ::
  ( Eq k,
    Hashable k,
    Lift k,
    Lift v
  ) =>
  HashMap k v ->
  TH.Q (TH.TExp (HashMap k v))
#endif
liftTypedHashMap hm =
  [||HashMap.fromList $$(liftTyped $ HashMap.toList hm)||]
