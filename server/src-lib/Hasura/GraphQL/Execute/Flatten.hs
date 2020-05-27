{-# LANGUAGE StrictData #-}

module Hasura.GraphQL.Execute.Flatten
  ( flattenSelectionSet
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended  as Map

import           Control.Lens
import           Language.GraphQL.Draft.Syntax

import           Hasura.RQL.Types.Error
import           Hasura.Server.Utils
import           Hasura.SQL.Types

-- | Internal bookkeeping used during flattening.
data FlattenEnv = FlattenEnv
  { _feFragmentDefinitions :: HashMap Name FragmentDefinition
  -- ^ All known fragment definitions.
  , _feFragmentStack       :: [Name]
  -- ^ Fragments we’re currently flattening higher up in the call stack, used to
  -- detect fragment cycles.
  }

-- | Internal bookkeeping used during flattening.
newtype FlattenState var = FlattenState
  { _fsFragmentCache :: HashMap Name (InlineFragment NoFragments var)
  -- ^ A cache of fragment definitions we’ve already flattened, so we don’t need
  -- to flatten them again.
  }

$(makeLensesFor [("_feFragmentStack", "feFragmentStack")] ''FlattenEnv)
$(makeLenses ''FlattenState)

type MonadFlatten var m =
  ( MonadError QErr m
  , MonadReader FlattenEnv m
  , MonadState (FlattenState var) m )

flattenSelectionSet
  :: MonadError QErr m
  => [FragmentDefinition]
  -> SelectionSet FragmentSpread var
  -> m (SelectionSet NoFragments var)
flattenSelectionSet fragmentDefinitions selectionSet
  = traverse flattenSelection selectionSet
  & flip evalStateT FlattenState{ _fsFragmentCache = mempty }
  & flip runReaderT FlattenEnv
    { _feFragmentDefinitions = Map.fromListOn _fdName fragmentDefinitions
    , _feFragmentStack = [] }

flattenSelection
  :: MonadFlatten var m
  => Selection FragmentSpread var
  -> m (Selection NoFragments var)
flattenSelection (SelectionField field@Field{ _fSelectionSet }) = do
  selectionSet <- traverse flattenSelection _fSelectionSet
  pure $! SelectionField field{ _fSelectionSet = selectionSet }
flattenSelection (SelectionFragmentSpread spread) =
  SelectionInlineFragment <$> inlineFragmentSpread spread
flattenSelection (SelectionInlineFragment fragment@InlineFragment{ _ifSelectionSet }) = do
  selectionSet <- traverse flattenSelection _ifSelectionSet
  pure $! SelectionInlineFragment fragment{ _ifSelectionSet = selectionSet }

inlineFragmentSpread
  :: forall var m. MonadFlatten var m
  => FragmentSpread var
  -> m (InlineFragment NoFragments var)
inlineFragmentSpread FragmentSpread{ _fsName, _fsDirectives } = do
  FlattenEnv{ _feFragmentDefinitions, _feFragmentStack } <- ask
  FlattenState{ _fsFragmentCache } <- get

  if -- If we’ve already flattened this fragment, no need to flatten it again.
     | Just fragment <- Map.lookup _fsName _fsFragmentCache ->
       pure $! addSpreadDirectives fragment

     -- Fragment cycles are always illegal; see
     -- http://spec.graphql.org/June2018/#sec-Fragment-spreads-must-not-form-cycles
     | (fragmentCycle, _:_) <- break (== _fsName) _feFragmentStack ->
       throw400 ValidationFailed $ "the fragment definition(s) "
         <> englishList "and" (dquoteTxt <$> (_fsName :| reverse fragmentCycle))
         <> " form a cycle"

     -- We didn’t hit the fragment cache, so look up the definition and flatten it.
     | Just FragmentDefinition{ _fdTypeCondition, _fdSelectionSet }
         <- Map.lookup _fsName _feFragmentDefinitions -> do

       selectionSet <- locally feFragmentStack (_fsName :) $
         traverse flattenSelection (fmap absurd <$> _fdSelectionSet)

       let fragment = InlineFragment
             { _ifTypeCondition = Just _fdTypeCondition
             -- As far as I can tell, the GraphQL spec says that directives
             -- on the fragment definition do NOT apply to the fields in its
             -- selection set.
             , _ifDirectives = []
             , _ifSelectionSet = selectionSet
             }
       modify' $ over fsFragmentCache $ Map.insert _fsName fragment
       pure $! addSpreadDirectives fragment

     -- If we get here, the fragment name is unbound; raise an error.
     | otherwise -> throw400 ValidationFailed $
       "reference to undefined fragment " <>> _fsName
  where
    addSpreadDirectives fragment =
      fragment{ _ifDirectives = _ifDirectives fragment ++ _fsDirectives }
