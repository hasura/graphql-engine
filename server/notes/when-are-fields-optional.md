This note is in [Hasura.GraphQL.Parser.Internal.Input](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Internal/Input.hs#L63).
It is referenced at:
  - line 70 of [Hasura.GraphQL.Parser.Internal.TypeChecking](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Internal/TypeChecking.hs#L70)

# When are fields optional?


When is an input field (i.e. an argument of a selection set field, or a field of
an input object) required to be specified? In fact, the GraphQL specification is
contradictory! As part of the discussion on what
"[Non-Null](http://spec.graphql.org/June2018/#sec-Type-System.Non-Null)" means,
it says:

> Inputs (such as field arguments), are always optional by default. However a
> non‐null input type is required. In addition to not accepting the value
> **null**, it also does not accept omission. For the sake of simplicity
> nullable types are always optional and non‐null types are always required.

However, under the [validity rules for field
arguments](http://spec.graphql.org/June2018/#sec-Required-Arguments), it says:

> Arguments can be required. An argument is required if the argument type is
> non‐null and does not have a default value. Otherwise, the argument is
> optional.

And under the [validity rules for input object
fields](http://spec.graphql.org/June2018/#sec-Input-Object-Required-Fields), it
says:

> Input object fields may be required. Much like a field may have required
> arguments, an input object may have required fields. An input field is
> required if it has a non‐null type and does not have a default
> value. Otherwise, the input object field is optional.

The first quote above is probably incorrect. Null values were introduced in
graphql/graphql-spec@3ce8b790da33f52f9258929258877a0a7768c620, when default
values already existed, and in particular the first quote above was already
written. Nullable types and values introduced ambiguity. There was an attempt to
resolve this ambiguity in
graphql/graphql-spec@0356f0cd105ca54cbdf5eb0f37da589eeac8c641, which introduced
the last two quotes above. However, clearly some ambiguity was left behind in
the spec.
