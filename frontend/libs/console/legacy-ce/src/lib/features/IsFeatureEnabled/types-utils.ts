import type { Opaque } from 'type-fest';
import type { IsEqual } from 'type-fest';
import type { ConditionalExcept } from 'type-fest';

// The following types come from types-fest' ConditionalPickDeep type. I ported them here because I
// needed to change the type of the properties that are not matching the condition (see the comment
// below).

type ConditionalPickDeepSymbol = Opaque<symbol, 'conditional-pick-deep-symbol'>;

type AssertCondition<
  Type,
  Condition,
  Options extends ConditionalPickDeepOptions
> = Options['condition'] extends 'equality'
  ? IsEqual<Type, Condition>
  : Type extends Condition
  ? true
  : false;

type ConditionalPickDeepOptions = {
  condition?: 'extends' | 'equality';
};

export type ConditionalPickDeepCompatibilityProperties<
  Type,
  Condition,
  Options extends ConditionalPickDeepOptions = {}
> = ConditionalSimplifyDeep<
  ConditionalExcept<
    {
      [Key in keyof Type]: AssertCondition<
        Type[Key],
        Condition,
        Options
      > extends true
        ? // --------------------------------------------------
          // --------------------------------------------------
          // This is the change compared to the types-fest original type. The original type allow to
          // maintain the original value of the property. Instead, I need the property to be
          // transformed to `true`
          // ? Type[Key]
          true // <-- changed to avoid getting the original type but the `true` type
        : // --------------------------------------------------
        // --------------------------------------------------
        Type[Key] extends object
        ? ConditionalPickDeepCompatibilityProperties<
            Type[Key],
            Condition,
            Options
          >
        : ConditionalPickDeepSymbol;
    },
    (ConditionalPickDeepSymbol | undefined) | Record<PropertyKey, never>
  >
>;

type ConditionalSimplifyDeep<
  Type,
  ExcludeType = never,
  IncludeType = unknown
> = Type extends ExcludeType
  ? Type
  : Type extends IncludeType
  ? {
      [TypeKey in keyof Type]: ConditionalSimplifyDeep<
        Type[TypeKey],
        ExcludeType,
        IncludeType
      >;
    }
  : Type;
