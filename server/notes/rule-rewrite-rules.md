This note is in [Hasura.Incremental.Internal.Rule](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Incremental/Internal/Rule.hs#L61).

# Rule rewrite rules

As explained by Note [Arrow rewrite rules] in Control.Arrow.Extended, it’s important to define
type-specific rewrite rules to get good performance with arrows when the concrete type is used. This
is especially important for `Rule`, since the recursive definitions of operations like `.` and `arr`
are very difficult for the optimizer to deal with, and the composition of lots of small rules
created with `arr` is very inefficient.

Since GHC aggressively specializes and inlines class methods, the rules cannot be defined on the
class methods themselves. Instead, the class methods expand to auxiliary definitions, and those
definitions include an INLINABLE[0] pragma that ensures they do not inline until the final
optimization phase. The rules are defined in terms of those definitions, so they will be able to do
their work in prior phases.

