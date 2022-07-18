This note is in [Hasura.GraphQL.Parser.Collect](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Collect.hs#L175).
It is referenced at:
  - line 171 of [Hasura.GraphQL.Parser.Collect](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Collect.hs#L171)

# Lazily merge selection sets

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
interpret their own inputs on an as-needed basis.
