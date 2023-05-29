{-# LANGUAGE TemplateHaskell #-}

-- | This module implements /fragment inlining/, which converts all fragment
-- spreads in a GraphQL query to inline fragments. For example, given a query like
--
-- > query {
-- >   users {
-- >     id
-- >     ...userFields
-- >   }
-- > }
-- >
-- > fragment userFields on User {
-- >   name
-- >   favoriteColor
-- > }
--
-- the fragment inliner will convert it to this:
--
-- > query {
-- >   users {
-- >     id
-- >     ... on User {
-- >       name
-- >       favoriteColor
-- >     }
-- >   }
-- > }
--
-- This is a straightforward and mechanical transformation, but it simplifies
-- further processing, since we catch unbound fragments and recursive fragment
-- definitions early in the pipeline, so parsing does not have to worry about it.
-- In that sense, fragment inlining is similar to the variable resolution pass
-- performed by "Hasura.GraphQL.Execute.Resolve", but for fragment definitions
-- rather than variables.
module Hasura.GraphQL.Execute.Inline
  ( InlineMT,
    InlineM,
    inlineSelectionSet,
    inlineField,
    runInlineMT,
    runInlineM,
  )
where

import Control.Lens
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.HashSet qualified as Set
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.Server.Utils
import Language.GraphQL.Draft.Syntax

-- | Internal bookkeeping used during inlining.
data InlineEnv = InlineEnv
  { -- | All known fragment definitions.
    _ieFragmentDefinitions :: HashMap Name FragmentDefinition,
    -- | Fragments we’re currently inlining higher up in the call stack, used to
    -- detect fragment cycles.
    _ieFragmentStack :: [Name]
  }

-- | Internal bookkeeping used during inlining.
newtype InlineState = InlineState
  { -- | A cache of fragment definitions we’ve already inlined, so we don’t need
    -- to inline them again.
    _isFragmentCache :: HashMap Name (InlineFragment NoFragments Name)
  }

$(makeLensesFor [("_ieFragmentStack", "ieFragmentStack")] ''InlineEnv)
$(makeLenses ''InlineState)

type MonadInline m =
  ( MonadError QErr m,
    MonadReader InlineEnv m,
    MonadState InlineState m
  )

type InlineMT m a = (MonadError QErr m) => (StateT InlineState (ReaderT InlineEnv m)) a

type InlineM a = InlineMT (Except QErr) a

{-# INLINE runInlineMT #-}
runInlineMT ::
  forall m a.
  (MonadError QErr m) =>
  HashMap Name FragmentDefinition ->
  InlineMT m a ->
  m a
runInlineMT uniqueFragmentDefinitions =
  flip
    runReaderT
    InlineEnv
      { _ieFragmentDefinitions = uniqueFragmentDefinitions,
        _ieFragmentStack = []
      }
    . flip evalStateT InlineState {_isFragmentCache = mempty}

{-# INLINE runInlineM #-}
runInlineM ::
  forall a.
  HashMap Name FragmentDefinition ->
  InlineM a ->
  Either QErr a
runInlineM fragments = runExcept . runInlineMT fragments

-- | Inlines all fragment spreads in a 'SelectionSet'; see the module
-- documentation for "Hasura.GraphQL.Execute.Inline" for details.
inlineSelectionSet ::
  (MonadError QErr m, Foldable t) =>
  t FragmentDefinition ->
  SelectionSet FragmentSpread Name ->
  m (SelectionSet NoFragments Name)
inlineSelectionSet fragmentDefinitions selectionSet = do
  let fragmentDefinitionMap = HashMap.groupOnNE _fdName fragmentDefinitions
  uniqueFragmentDefinitions <- flip
    HashMap.traverseWithKey
    fragmentDefinitionMap
    \fragmentName fragmentDefinitions' ->
      case fragmentDefinitions' of
        a :| [] -> return a
        _ -> throw400 ParseFailed $ "multiple definitions for fragment " <>> fragmentName
  let usedFragmentNames = Set.fromList $ fragmentsInSelectionSet selectionSet
      definedFragmentNames = Set.fromList $ HashMap.keys uniqueFragmentDefinitions
      -- At the time of writing, this check is disabled using
      -- a local binding because, the master branch doesn't implement this
      -- check.
      -- TODO: Do this check using a feature flag
      isFragmentValidationEnabled = False
  when (isFragmentValidationEnabled && (usedFragmentNames /= definedFragmentNames))
    $ throw400 ValidationFailed
    $ "following fragment(s) have been defined, but have not been used in the query - "
    <> T.concat
      ( L.intersperse ", "
          $ map unName
          $ Set.toList
          $ Set.difference definedFragmentNames usedFragmentNames
      )
  -- The below code is a manual inlining of 'runInlineMT', as appearently the
  -- inlining optimization does not trigger, even with the INLINE pragma.
  traverse inlineSelection selectionSet
    & flip evalStateT InlineState {_isFragmentCache = mempty}
    & flip
      runReaderT
      InlineEnv
        { _ieFragmentDefinitions = uniqueFragmentDefinitions,
          _ieFragmentStack = []
        }
  where
    fragmentsInSelectionSet :: SelectionSet FragmentSpread Name -> [Name]
    fragmentsInSelectionSet selectionSet' = concatMap getFragFromSelection selectionSet'

    getFragFromSelection :: Selection FragmentSpread Name -> [Name]
    getFragFromSelection = \case
      SelectionField fld -> fragmentsInSelectionSet $ _fSelectionSet fld
      SelectionFragmentSpread fragmentSpread -> [_fsName fragmentSpread]
      SelectionInlineFragment inlineFragment -> fragmentsInSelectionSet $ _ifSelectionSet inlineFragment

inlineSelection ::
  (MonadInline m) =>
  Selection FragmentSpread Name ->
  m (Selection NoFragments Name)
inlineSelection (SelectionField field) =
  withPathK "selectionSet" $ SelectionField <$> inlineField field
inlineSelection (SelectionFragmentSpread spread) =
  withPathK "selectionSet"
    $ SelectionInlineFragment
    <$> inlineFragmentSpread spread
inlineSelection (SelectionInlineFragment fragment@InlineFragment {_ifSelectionSet}) = do
  selectionSet <- traverse inlineSelection _ifSelectionSet
  pure $! SelectionInlineFragment fragment {_ifSelectionSet = selectionSet}

{-# INLINE inlineField #-}
inlineField :: (MonadInline m) => Field FragmentSpread Name -> m (Field NoFragments Name)
inlineField field@(Field {_fSelectionSet}) = withPathK (unName $ _fName field) $ do
  selectionSet <- traverse inlineSelection _fSelectionSet
  pure $! field {_fSelectionSet = selectionSet}

inlineFragmentSpread ::
  (MonadInline m) =>
  FragmentSpread Name ->
  m (InlineFragment NoFragments Name)
inlineFragmentSpread FragmentSpread {_fsName, _fsDirectives} = do
  InlineEnv {_ieFragmentDefinitions, _ieFragmentStack} <- ask
  InlineState {_isFragmentCache} <- get

  if
    -- If we’ve already inlined this fragment, no need to process it again.
    | Just fragment <- HashMap.lookup _fsName _isFragmentCache ->
        pure $! addSpreadDirectives fragment
    -- Fragment cycles are always illegal; see
    -- http://spec.graphql.org/June2018/#sec-Fragment-spreads-must-not-form-cycles
    | (fragmentCycle, _ : _) <- break (== _fsName) _ieFragmentStack ->
        throw400 ValidationFailed
          $ "the fragment definition(s) "
          <> englishList "and" (toTxt <$> (_fsName :| reverse fragmentCycle))
          <> " form a cycle"
    -- We didn’t hit the fragment cache, so look up the definition and convert
    -- it to an inline fragment.
    | Just FragmentDefinition {_fdTypeCondition, _fdSelectionSet} <-
        HashMap.lookup _fsName _ieFragmentDefinitions -> withPathK (unName _fsName) $ do
        selectionSet <-
          locally ieFragmentStack (_fsName :)
            $ traverse inlineSelection _fdSelectionSet

        let fragment =
              InlineFragment
                { _ifTypeCondition = Just _fdTypeCondition,
                  -- As far as I can tell, the GraphQL spec says that directives
                  -- on the fragment definition do NOT apply to the fields in its
                  -- selection set.
                  _ifDirectives = [],
                  _ifSelectionSet = selectionSet
                }
        modify' $ over isFragmentCache $ HashMap.insert _fsName fragment
        pure $! addSpreadDirectives fragment

    -- If we get here, the fragment name is unbound; raise an error.
    -- http://spec.graphql.org/June2018/#sec-Fragment-spread-target-defined
    | otherwise ->
        throw400 ValidationFailed
          $ "reference to undefined fragment "
          <>> _fsName
  where
    addSpreadDirectives fragment =
      fragment {_ifDirectives = _ifDirectives fragment ++ _fsDirectives}
