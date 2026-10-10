import type { ConditionalPickDeepCompatibilityProperties } from './types-utils';

// Compile-time assertions for ConditionalPickDeepCompatibilityProperties and,
// transitively, the `unique symbol` exclusion marker. legacy-ce has no
// typecheck nx target (the build transpiles via babel), so these are verified
// with `tsc --noEmit` at review time; the `export type` aliases keep every
// assertion referenced (no unused-type lint).
type Expect<T extends true> = T;
type Equal<X, Y> =
  (<T>() => T extends X ? 1 : 2) extends <T>() => T extends Y ? 1 : 2
    ? true
    : false;

type Sample = {
  topMatch: 'enabled';
  topMiss: 'disabled';
  nested: { innerMatch: 'enabled'; innerMiss: number };
};
type Picked = ConditionalPickDeepCompatibilityProperties<Sample, 'enabled'>;

// matching leaf values are rewritten to `true`...
export type _TopMatchIsTrue = Expect<Equal<Picked['topMatch'], true>>;
// ...recursively, inside nested objects
export type _NestedInnerIsTrue = Expect<
  Equal<Picked['nested']['innerMatch'], true>
>;
// non-matching values are excluded (dropped from the resulting keys)...
export type _TopMissExcluded = Expect<
  Equal<'topMiss' extends keyof Picked ? true : false, false>
>;
// ...including non-matching leaves nested inside a kept object
export type _NestedInnerMissExcluded = Expect<
  Equal<'innerMiss' extends keyof Picked['nested'] ? true : false, false>
>;
