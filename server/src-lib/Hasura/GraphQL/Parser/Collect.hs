{-# LANGUAGE StrictData #-}

{-| This module implements two parts of the GraphQL specification:

  1. <§ 5.3.2 Field Selection Merging http://spec.graphql.org/June2018/#sec-Field-Selection-Merging>
  2. <§ 6.3.2 Field Collection http://spec.graphql.org/June2018/#sec-Field-Collection>

These are described in completely different sections of the specification, but
they’re actually highly related: both essentially normalize fields in a
selection set. -}
module Hasura.GraphQL.Parser.Collect
  ( collectFields
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended          as Map
import qualified Data.HashMap.Strict.InsOrd            as OMap

import           Language.GraphQL.Draft.Syntax

import           Hasura.GraphQL.Parser.Class
import {-# SOURCE #-} Hasura.GraphQL.Parser.Internal.Parser (boolean, runParser)
import           Hasura.GraphQL.Parser.Schema
import           Hasura.SQL.Types

-- | Collects the effective set of fields queried by a selection set by
-- flattening fragments and merging duplicate fields.
collectFields
  :: (MonadParse m, Foldable t)
  => t Name
  -- ^ The names of the object types and interface types the 'SelectionSet' is
  -- selecting against.
  -> SelectionSet NoFragments Variable
  -> m (InsOrdHashMap Name (Field NoFragments Variable))
collectFields objectTypeNames selectionSet =
  mergeFields =<< flattenSelectionSet objectTypeNames selectionSet

-- | Flattens inline fragments in a selection set. For example,
--
-- > {
-- >   bar
-- >   ... on Foo {
-- >     baz
-- >     qux
-- >   }
-- > }
--
-- is flattened to:
--
-- > {
-- >   bar
-- >   baz
-- >   qux
-- > }
--
-- Nested fragments are similarly flattened, but only as is necessary: fragments
-- inside subselection sets of individual fields are /not/ flattened. For
-- example,
--
-- > {
-- >   bar
-- >   ... on Foo {
-- >     baz {
-- >       ... on Baz {
-- >         foo
-- >       }
-- >     }
-- >     qux
-- >   }
-- > }
--
-- is flattened to
--
-- > {
-- >   bar
-- >   baz {
-- >     ... on Baz {
-- >       foo
-- >     }
-- >   }
-- >   qux
-- > }
--
-- leaving the innermost fragment on @baz@ alone.
--
-- This function also applies @\@include@ and @\@skip@ directives, since they
-- should be applied before fragments are flattened.
flattenSelectionSet
  :: (MonadParse m, Foldable t)
  => t Name
  -- ^ The name of the object type the 'SelectionSet' is selecting against.
  -> SelectionSet NoFragments Variable
  -> m [Field NoFragments Variable]
flattenSelectionSet objectTypeNames = fmap concat . traverse flattenSelection
  where
    -- The easy case: just a single field.
    flattenSelection (SelectionField field) =
      applyInclusionDirectives (_fDirectives field) $ pure [field]
    -- This case has already been eliminated by the fragment inliner.
    flattenSelection (SelectionFragmentSpread fragment) = case fragment of {}
    -- The involved case: we have an inline fragment to process.
    flattenSelection (SelectionInlineFragment fragment) =
      applyInclusionDirectives (_ifDirectives fragment)
      case _ifTypeCondition fragment of
        -- No type condition, so the fragment unconditionally applies.
        Nothing -> flattenInlineFragment fragment
        Just typeName
          -- There is a type condition, but it is just the type of the
          -- selection set; the fragment trivially applies.
          | typeName `elem` objectTypeNames -> flattenInlineFragment fragment

          -- TODO implement this check:
          -- http://spec.graphql.org/June2018/#sec-Fragment-Spread-Type-Existence

          -- TODO we are now starting to support interfaces and unions; fix the below

          -- Otherwise, the fragment must not apply, because we do not currently
          -- support interfaces or unions. According to the GraphQL spec, it is
          -- an *error* to select a fragment that cannot possibly apply to the
          -- given type; see
          -- http://spec.graphql.org/June2018/#sec-Fragment-spread-is-possible.
          -- Therefore, we raise an error.
          | otherwise -> return []
            {- parseError $ "illegal type condition in fragment; type "
              <> typeName <<> " is unrelated to any of the types " <>
              Text.intercalate ", " (fmap dquoteTxt (toList objectTypeNames))
            -}

    flattenInlineFragment InlineFragment{ _ifSelectionSet } =
      flattenSelectionSet objectTypeNames _ifSelectionSet

    applyInclusionDirectives directives continue
      | Just directive <- find ((== $$(litName "include")) . _dName) directives
      = applyInclusionDirective id directive continue
      | Just directive <- find ((== $$(litName "skip")) . _dName) directives
      = applyInclusionDirective not directive continue
      | otherwise = continue

    applyInclusionDirective adjust Directive{ _dName, _dArguments } continue = do
      ifArgument <- Map.lookup $$(litName "if") _dArguments `onNothing`
        parseError ("missing \"if\" argument for " <> _dName <<> " directive")
      value <- runParser boolean ifArgument
      if adjust value then continue else pure []

-- | Merges fields according to the rules in the GraphQL specification, specifically
-- <§ 5.3.2 Field Selection Merging http://spec.graphql.org/June2018/#sec-Field-Selection-Merging>.
mergeFields
  :: (MonadParse m, Eq var)
  => [Field NoFragments var]
  -> m (InsOrdHashMap Name (Field NoFragments var))
mergeFields = foldM addField OMap.empty
  where
    addField fields newField = case OMap.lookup alias fields of
      Nothing ->
        pure $! OMap.insert alias newField fields
      Just oldField -> do
        mergedField <- mergeField alias oldField newField
        pure $! OMap.insert alias mergedField fields
      where
        alias = fromMaybe (_fName newField) (_fAlias newField)

    mergeField alias oldField newField = do
      unless (_fName oldField == _fName newField) $ parseError $
        "selection of both " <> _fName oldField <<> " and " <>
         _fName newField <<> " specify the same response name, " <>> alias

      unless (_fArguments oldField == _fArguments newField) $ parseError $
        "inconsistent arguments between multiple selections of " <>
        "field " <>> _fName oldField

      pure $! Field
        { _fAlias = Just alias
        , _fName = _fName oldField
        , _fArguments = _fArguments oldField
        -- see Note [Drop directives from merged fields]
        , _fDirectives = []
        -- see Note [Lazily merge selection sets]
        , _fSelectionSet = _fSelectionSet oldField ++ _fSelectionSet newField
        }

{- Note [Drop directives from merged fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we merge two fields, what do we do with directives? The GraphQL spec isn’t
very clear here, but it does explicitly state that directives only need to be
unique per unmerged field (§ 5.7.3 Directives Are Unique Per Location,
http://spec.graphql.org/June2018/#sec-Directives-Are-Unique-Per-Location). For
clarity, here is the example given by the spec:

    query ($foo: Boolean = true, $bar: Boolean = false) {
      field @skip(if: $foo) {
        subfieldA
      }
      field @skip(if: $bar) {
        subfieldB
      }
    }

The spec says this is totally fine, since the @skip directives appear in
different places. This forces our hand: we *must* process @include/@skip
directives prior to merging fields. And conveniently, aside from @include/@skip,
we don’t care about directives, so we don’t bother reconciling them during field
merging---we just drop them.

Note [Lazily merge selection sets]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Field merging is described in a recursive way in the GraphQL spec (§ 5.3.2 Field
Selection Merging http://spec.graphql.org/June2018/#sec-Field-Selection-Merging).
This makes sense: if fields have sub-selection sets, they should be recursively
merged. For example, suppose we have the following selection set:

    {
      field1 {
        field2 {
          field3
        }
        field5
      }
      field1 {
        field2 {
          field4
        }
        field5
      }
    }

After a single level of merging, we’ll merge the two occurrences of field1
together to get:

    {
      field1 {
        field2 {
          field3
        }
        field5
        field2 {
          field4
        }
        field5
      }
    }

It would be natural to then merge the inner selection set, too, yielding:

    {
      field1 {
        field2 {
          field3
          field4
        }
        field5
      }
    }

But we don’t do this. Instead, we stop after the first level of merging, so
field1’s sub-selection set still has duplication. Why? Because recursively
merging fields would also require recursively flattening fragments, and
flattening fragments is tricky: it requires knowledge of type information.

Fortunately, this lazy approach to field merging is totally okay, because we
call collectFields (and therefore mergeFields) each time we parse a selection
set. Once we get to processing the sub-selection set of field1, we’ll call
collectFields again, and it will merge things the rest of the way. This is
consistent with the way the rest of our parsing system works, where parsers
interpret their own inputs on an as-needed basis. -}
