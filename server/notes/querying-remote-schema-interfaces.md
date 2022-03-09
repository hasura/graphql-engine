This note is in [Hasura.GraphQL.Schema.Remote](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Schema/Remote.hs#L683).
It is referenced at:
  - line 733 of [Hasura.GraphQL.Schema.Remote](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Schema/Remote.hs#L733)

# Querying remote schema interfaces

When querying Remote schema interfaces, we need to re-construct
the incoming query to be compliant with the upstream remote.
We need to do this because the `SelectionSet`(s) that are
inputted to this function have the fragments (if any) flattened.
(Check `flattenSelectionSet` in 'Hasura.GraphQL.Parser.Collect' module)
The `constructInterfaceSelectionSet` function makes a valid interface query by:
1. Getting the common interface fields in all the selection sets
2. Remove the common fields obtained in #1 from the selection sets
3. Construct a selection field for every common interface field
4. Construct inline fragments for non-common interface fields
   using the result of #2 for every object
5. Construct the final selection set by combining #3 and #4

Example: Suppose an interface 'Character' is defined in the upstream
and two objects 'Human' and 'Droid' implement the 'Character' Interface.

Suppose, a field 'hero' returns 'Character'.

{
   hero {
     id
     name
     ... on Droid {
       primaryFunction
     }
     ... on Human {
       homePlanet
     }
   }
}

When we parse the selection set of the `hero` field, we parse the selection set
twice: once for the `Droid` object type, which would be passed a selection set
containing the field(s) defined in the `Droid` object type and similarly once
for the 'Human' object type. The result of the interface selection set parsing
would then be the results of the parsing of the object types when passed their
corresponding flattened selection sets and the results of the parsing of the
interface fields.

After we parse the above GraphQL query, we get a selection set containing
the interface fields and the selection sets of the objects that were queried
in the GraphQL query. Since, we have the selection sets of the objects that
were being queried, we can convert them into inline fragments resembling
the original query and then query the remote schema with the newly
constructed query.

