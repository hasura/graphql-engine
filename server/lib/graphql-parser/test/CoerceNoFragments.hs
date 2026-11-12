-- | Checks the 'unsafeCoerce' in 'coerceNoFragments' against the safe
-- conversion via 'traverseSelectionSetFragment'.
module CoerceNoFragments
  ( coerceNoFragmentsTests,
  )
where

-------------------------------------------------------------------------------

import Data.Maybe (isJust, isNothing)
import Hedgehog
  ( Gen,
    Property,
    PropertyName,
    TestLimit,
    cover,
    forAll,
    property,
    withTests,
    (===),
  )
import Hedgehog.Gen qualified as Gen
import Language.GraphQL.Draft.Generator (genSelectionSet)
import Language.GraphQL.Draft.Syntax
import Prelude

-------------------------------------------------------------------------------

coerceNoFragmentsTests :: TestLimit -> [(PropertyName, Property)]
coerceNoFragmentsTests nTests =
  [ ( "property [ coerceNoFragments s == traverseSelectionSetFragment (const Nothing) s ]",
      withTests nTests propCoerceNoFragments
    )
  ]

propCoerceNoFragments :: Property
propCoerceNoFragments = property do
  selectionSet <- forAll genSelectionSetMaybeWithoutSpreads
  let expected = traverseSelectionSetFragment (const Nothing) selectionSet
  cover 40 "no fragment spreads" $ isJust expected
  cover 40 "fragment spreads" $ isNothing expected
  coerceNoFragments selectionSet === expected

-- | 'genSelectionSet' always generates fragment spreads at the leaves, so for
-- half of the generated selection sets we replace them all with leaf fields.
genSelectionSetMaybeWithoutSpreads :: Gen (SelectionSet FragmentSpread Name)
genSelectionSetMaybeWithoutSpreads = do
  selectionSet <- genSelectionSet
  withoutSpreads <- Gen.bool
  pure if withoutSpreads then removeSpreads selectionSet else selectionSet

removeSpreads :: SelectionSet FragmentSpread var -> SelectionSet FragmentSpread var
removeSpreads = map \case
  SelectionField field ->
    SelectionField field {_fSelectionSet = removeSpreads (_fSelectionSet field)}
  SelectionFragmentSpread FragmentSpread {_fsName, _fsDirectives} ->
    SelectionField (Field Nothing _fsName mempty _fsDirectives [])
  SelectionInlineFragment inlineFragment ->
    SelectionInlineFragment inlineFragment {_ifSelectionSet = removeSpreads (_ifSelectionSet inlineFragment)}
