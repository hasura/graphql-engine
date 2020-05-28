{-# LANGUAGE StrictData #-}

{-| This module implements /fragment inlining/, which converts all fragment
spreads in a GraphQL query to inline fragments. For example, given a query like

> query {
>   users {
>     id
>     ...userFields
>   }
> }
>
> fragment userFields on User {
>   name
>   favoriteColor
> }

the fragment inliner will convert it to this:

> query {
>   users {
>     id
>     ... on User {
>       name
>       favoriteColor
>     }
>   }
> }

This is a straightforward and mechanical transformation, but it simplifies
further processing, since we catch unbound fragments and recursive fragment
definitions early in the pipeline, so parsing does not have to worry about it.
In that sense, fragment inlining is similar to the variable resolution pass
performed by "Hasura.GraphQL.Execute.Resolve", but for fragment definitions
rather than variables. -}
module Hasura.GraphQL.Execute.Inline
  ( inlineSelectionSet
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended  as Map

import           Control.Lens
import           Language.GraphQL.Draft.Syntax

import           Hasura.RQL.Types.Error
import           Hasura.Server.Utils
import           Hasura.SQL.Types

-- | Internal bookkeeping used during inlining.
data InlineEnv = InlineEnv
  { _ieFragmentDefinitions :: HashMap Name FragmentDefinition
  -- ^ All known fragment definitions.
  , _ieFragmentStack       :: [Name]
  -- ^ Fragments we’re currently inlining higher up in the call stack, used to
  -- detect fragment cycles.
  }

-- | Internal bookkeeping used during inlining.
newtype InlineState var = InlineState
  { _isFragmentCache :: HashMap Name (InlineFragment NoFragments var)
  -- ^ A cache of fragment definitions we’ve already inlined, so we don’t need
  -- to inline them again.
  }

$(makeLensesFor [("_ieFragmentStack", "ieFragmentStack")] ''InlineEnv)
$(makeLenses ''InlineState)

type MonadInline var m =
  ( MonadError QErr m
  , MonadReader InlineEnv m
  , MonadState (InlineState var) m )

-- | Inlines all fragment spreads in a 'SelectionSet'; see the module
-- documentation for "Hasura.GraphQL.Execute.Inline" for details.
inlineSelectionSet
  :: MonadError QErr m
  => [FragmentDefinition]
  -> SelectionSet FragmentSpread var
  -> m (SelectionSet NoFragments var)
inlineSelectionSet fragmentDefinitions selectionSet
  = traverse inlineSelection selectionSet
  & flip evalStateT InlineState{ _isFragmentCache = mempty }
  & flip runReaderT InlineEnv
    { _ieFragmentDefinitions = Map.fromListOn _fdName fragmentDefinitions
    , _ieFragmentStack = [] }

inlineSelection
  :: MonadInline var m
  => Selection FragmentSpread var
  -> m (Selection NoFragments var)
inlineSelection (SelectionField field@Field{ _fSelectionSet }) = do
  selectionSet <- traverse inlineSelection _fSelectionSet
  pure $! SelectionField field{ _fSelectionSet = selectionSet }
inlineSelection (SelectionFragmentSpread spread) =
  SelectionInlineFragment <$> inlineFragmentSpread spread
inlineSelection (SelectionInlineFragment fragment@InlineFragment{ _ifSelectionSet }) = do
  selectionSet <- traverse inlineSelection _ifSelectionSet
  pure $! SelectionInlineFragment fragment{ _ifSelectionSet = selectionSet }

inlineFragmentSpread
  :: MonadInline var m
  => FragmentSpread var
  -> m (InlineFragment NoFragments var)
inlineFragmentSpread FragmentSpread{ _fsName, _fsDirectives } = do
  InlineEnv{ _ieFragmentDefinitions, _ieFragmentStack } <- ask
  InlineState{ _isFragmentCache } <- get

  if -- If we’ve already inlined this fragment, no need to process it again.
     | Just fragment <- Map.lookup _fsName _isFragmentCache ->
       pure $! addSpreadDirectives fragment

     -- Fragment cycles are always illegal; see
     -- http://spec.graphql.org/June2018/#sec-Fragment-spreads-must-not-form-cycles
     | (fragmentCycle, _:_) <- break (== _fsName) _ieFragmentStack ->
       throw400 ValidationFailed $ "the fragment definition(s) "
         <> englishList "and" (dquoteTxt <$> (_fsName :| reverse fragmentCycle))
         <> " form a cycle"

     -- We didn’t hit the fragment cache, so look up the definition and convert
     -- it to an inline fragment.
     | Just FragmentDefinition{ _fdTypeCondition, _fdSelectionSet }
         <- Map.lookup _fsName _ieFragmentDefinitions -> do

       selectionSet <- locally ieFragmentStack (_fsName :) $
         traverse inlineSelection (fmap absurd <$> _fdSelectionSet)

       let fragment = InlineFragment
             { _ifTypeCondition = Just _fdTypeCondition
             -- As far as I can tell, the GraphQL spec says that directives
             -- on the fragment definition do NOT apply to the fields in its
             -- selection set.
             , _ifDirectives = []
             , _ifSelectionSet = selectionSet
             }
       modify' $ over isFragmentCache $ Map.insert _fsName fragment
       pure $! addSpreadDirectives fragment

     -- If we get here, the fragment name is unbound; raise an error.
     | otherwise -> throw400 ValidationFailed $
       "reference to undefined fragment " <>> _fsName
  where
    addSpreadDirectives fragment =
      fragment{ _ifDirectives = _ifDirectives fragment ++ _fsDirectives }
