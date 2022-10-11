# Building the schema

We use the same piece of code to generate the GraphQL schema and parse
it, to ensure those two parts of the code are always consistent. In
practice, we do this by building _introspectable_ parsers, in the
style of parser combinators, which turn an incoming GraphQL AST into
our internal representation ([IR](#ir)).

### Table of contents

<!--
Please make sure you update the table of contents when modifying this file. If
you're using emacs, you can generate a default version of it with `M-x
markdown-toc-refresh-toc` (provided by the package markdown-toc), and then edit
it for readability.
-->

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

- [Terminology](#terminology)
- [The `Parser` type](#the-parser-type)
- [Output](#output)
- [Input](#input)
- [Recursion, and tying the knot](#recursion-and-tying-the-knot)
- [The GraphQL context](#the-graphql-context)

<!-- markdown-toc end -->

## Terminology

The schema building code takes as input our metadata: what sources do
we have, what tables are tracked, what columns do they have... and
builds the corresponding GraphQL schema. More precisely, its output is
a series of _parsers_. That term is controversial, as it is ambiguous.

To clarify: an incoming request is first parsed from a raw string into
a GraphQL AST using our
[graphql-parser-hs](https://github.com/hasura/graphql-parser-hs)
library. At that point, no semantic analysis is performed: the output
of that phase will be a GraphQL document: a simple AST, on which no
semantic verification has been performed. The second step is to apply
those "schema parsers": their input is that GraphQL AST, and their
output is a semantic representation of the query (see the
[Output](#output) section).

To summarize: ahead of time, based on our metadata, we generate
*schema parsers*: they will parse an incoming GraphQL AST into a
transformed semantic AST, based on whether that incoming query is
consistent with our metadata.

## The `Parser` type

We have different types depending on what we're parsing: `Parser` for types in
the GraphQL schema, `FieldParser` for a field in an output type, and
`InputFieldsParser` for field in input types. But all three of them share a
similar structure: they combine static type information, and the actual parsing
function:

```haskell
data Parser n a = Parser
  { parserType :: TypeInfo
  , parserFunc :: ParserInput -> n a
  }
```

The GraphQL schema is a graph of types, stemming from one of the three roots: the
`query_root`, `mutation_root`, and `subscription_root` types. Consequently, if
we correctly build the `Parser` for the `query_root` type, then its `TypeInfo`
will be a "root" of the full graph.

What our combinators do is recursively build both at the same time. For
instance, consider `nullable` from `Hasura.GraphQL.Parser.Internal.Parser` (here
simplified a bit for readability):

```haskell
nullable :: MonadParse m => Parser m a -> Parser m (Maybe a)
nullable parser = Parser
  { pType = nullableType $ pType parser
  , pParser = \case
      JSONValue A.Null   -> pure Nothing
      GraphQLValue VNull -> pure Nothing
      value              -> Just <$> pParser parser value
  }
```

Given a parser for a value type `a`, this function translates it into a parser
of `Maybe a` that tolerates "null" values and updates its internal type
information to transform the corresponding GraphQL non-nullable `TypeName!` into
a nullable `TypeName`.

## Output

While the parsers keep track of the GraphQL types, their output is our IR: we
transform the incoming GrapQL AST into a semantic representation. This is
clearly visible with input fields, like in field arguments. For instance, this
is the definition of the parser for the arguments to a table (here again
slightly simplified for readability):

```haskell
tableArguments
  :: (MonadSchema m, MonadParse n)
  => SourceName     -- ^ name of the source we're building the schema for
  -> TableInfo b    -- ^ internal information about the given table (e.g. columns)
  -> SelPermInfo b  -- ^ selection permissions for that table
  -> m (
       -- parser for a group of input fields, such as arguments to a field
       -- has an Applicative instance to allow to write one parser for a group of
       -- arguments
       InputFieldsParser n (
         -- internal representation of the arguments to a table select
         IR.SelectArgs b
       )
     )
tableArguments sourceName tableInfo selectPermissions = do
  -- construct other parsers in the outer `m` monad
  whereParser    <- tableWhereArg    sourceName tableInfo selectPermissions
  orderByParser  <- tableOrderByArg  sourceName tableInfo selectPermissions
  distinctParser <- tableDistinctArg sourceName tableInfo selectPermissions
  -- combine them using an "applicative do"
  pure do
    whereArg    <- whereParser
    orderByArg  <- orderByParser
    limitArg    <- tableLimitArg
    offsetArg   <- tableOffsetArg
    distinctArg <- distinctParser
    pure $ IR.SelectArgs
      { IR._saWhere    = whereArg
      , IR._saOrderBy  = orderByArg
      , IR._saLimit    = limitArg
      , IR._saOffset   = offsetArg
      , IR._saDistinct = distinctArg
      }
```

Running the parser on the input will yield the `SelectArgs`; if used for a field
name `article`, it will result in the following GraphQL schema, if introspected
(null fields omitted for brevity):

```json
{
  "fields": [
    {
      "name": "article",
      "args": [
        {
          "name": "distinct_on",
          "type": {
            "name": null,
            "kind": "LIST",
            "ofType": {
              "name": null,
              "kind": "NON_NULL",
              "ofType": {
                "name": "article_select_column",
                "kind": "ENUM"
              }
            }
          }
        },
        {
          "name": "limit",
          "type": {
            "name": "Int",
            "kind": "SCALAR",
            "ofType": null
          }
        },
        {
          "name": "offset",
          "type": {
            "name": "Int",
            "kind": "SCALAR",
            "ofType": null
          }
        },
        {
          "name": "order_by",
          "type": {
            "name": null,
            "kind": "LIST",
            "ofType": {
              "name": null,
              "kind": "NON_NULL",
              "ofType": {
                "name": "article_order_by",
                "kind": "INPUT_OBJECT"
              }
            }
          }
        },
        {
          "name": "where",
          "type": {
            "name": "article_bool_exp",
            "kind": "INPUT_OBJECT",
            "ofType": null
          }
        }
      ]
    }
  ]
}
```

## Input

There is a noteworthy peculiarity with the input of the parsers for
GraphQL input types: some of the values we parse are JSON values,
supplied to a query by means of variable assignment:

```graphql
mutation($name: String!, $shape: geometry!) {
  insert_location_one(object: {name: $name, shape: $shape}) {
    id
  }
}
```

The GraphQL spec doesn't mandate a transport format for the variables;
the fact that they are encoding using JSON is a choice on our
part. However, this poses a problem: a variable's JSON value might not
be representable as a GraphQL value, as GraphQL values are a strict
subset of JSON values. For instance, the JSON object `{"1": "2"}` is
not representable in GraphQL, as `"1"` is not a valid key. This is not
an issue, as the spec doesn't mandate that the variables be translated
into GraphQL values; their parsing and validation is left entirely to
the service. However, it means that we need to keep track, when
parsing input values, of whether that value is coming from a GraphQL
literal or from a JSON value. Furthermore, we delay the expansion of
variables, in order to do proper type-checking.

Consequently, we represent input variables with the following type
(defined in `Hasura.GraphQL.Parser.Schema`):

```haskell
data InputValue v
  = GraphQLValue (G.Value v)
  | JSONValue J.Value
```

Whenever we need to inspect an input value, we usually start by "peeling the
variable" (see `peelVariable` in `Hasura.GraphQL.Parser.Internal.TypeChecking`),
to guarantee that we have an actual literal to inspect; we still end up with
either a GraphQL value, or a JSON value; parsers usually deal with both, such as
`nullable` (see section [The Parser Type](#the-parser-type)), or like scalar
parsers do (see `Hasura.GraphQL.Parser.Internal.Scalars`):

```haskell
float :: MonadParse m => Parser 'Both m Double
float = Parser
  { pType = schemaType
  , pParser =
      -- we first unpack the variable, if any:
      peelVariable (toGraphQLType schemaType) >=> \case
        -- we deal with valid GraphQL values
        GraphQLValue (VFloat f)   -> convertWith scientificToFloat f
        GraphQLValue (VInt   i)   -> convertWith scientificToFloat $ fromInteger i
        -- we deal with valid JSON values
        JSONValue    (A.Number n) -> convertWith scientificToFloat n
        -- we reject everything else
        v                         -> typeMismatch floatScalar "a float" v
  }
  where
    schemaType = NonNullable $ TNamed $ Definition "Float" Nothing TIScalar
```

This allows us to incrementally unpack JSON values without having to fully
transform them into GraphQL values in the first place; the following is
therefore accepted:

```
# graphql
query($w: foo_bool_exp!) {
  foo(where: $w) {
    id
  }
}

# json
{
  "w": {
    # graphql boolean expression
    "foo_json_field": {
      "_eq": {
        # json value that cannot be translated
        "1": "2"
      }
    }
  }
}
```

The parsers will unpack the variable into the `JSONValue` constructor, and the
`object` combinator will unpack the fields when parsing a boolean expression;
but the remaining `JSONValue $ Object [("1", String "2")]` will not be
translated into a GraphQL value, and parsed directly from JSON by the
appropriate value parser.

Step-by step:
- the value given to our `where` argument is a `GraphQLValue`, that
  contains a `VVariable` and its JSON value;
- when parsing the argument's `foo_bool_exp` type, we expect an
  object: we "peel" the variable, and our input value becomes a
  `JSONValue`, containing one entry, `"foo_json_field"`;
- we then parse each field one by one; to parse our one field, we
  first focus our input value on the actual field, and refine our
  input value to the content of thee field: `JSONValue $ Object
  [("_eq", Object [("1", String "2")])]`;
- that field's argument is a boolean expression, which is also an
  object: we repeat the same process;
- when finally parsing the argument to `_eq`, we are no longer in the
  realm of GraphQL syntax: the argument to `_eq` is whatever a value
  of that database column is; we use the appropriate column parser to
  interpret `{"1": "2"}` without treating is as a GraphQL value.

## Recursion, and tying the knot

One major hurdle that we face when building the schema is that, due to
relationships, our schema is not a tree, but a graph. Consider for instance two
tables, `author` and `article`, joined by two opposite relationships (one-to-one
AKA "object relationship, and one-to-many AKA "array relationship",
respectively); the GraphQL schema will end up with something akin to:

```graphql
type Author {
  id: Int!
  name: String!
  articles: [Article!]!
}

type Article {
  id: Int!
  name: String!
  author: Author!
}
```

To build the schema parsers for a query that selects those tables, we are going to
end up with code that would be essentially equivalent to:

```haskell
selectTable tableName tableInfo = do
  arguments    <- tableArguments tableInfo
  selectionSet <- traverse mkField $ fields tableInfo
  pure $ selection tableName arguments selectionSet

mkField = \case
  TableColumn c  -> field_ (name c)
  Relationship r -> field (name r) $ selectTable (target r)
```

We would end up with an infinite recursion building the parsers:
```
-> selectTable "author"
  -> mkField "articles"
    -> selectTable "article"
      -> mkField "author"
        -> selectTable "author"
```

To avoid this, we *memoize* the parsers as we build them. This is, however,
quite tricky: since building a parser might require it knowing about itself, we
cannot memoize it after it's build; we have to memoize it *before*. What we end
up doing is relying on `unsafeInterleaveIO` to store in the memoization cache a
value whose evaluation will be delayed, and that can be updated after we're done
evaluating the parser. The relevant code lives in `Hasura.GraphQL.Parser.Monad`.

This all works just fine as long as building a parser doesn't require forcing
its own evaluation: as long as the newly built parser only references fields to
itself, the graph will be properly constructed, and the knot will be tied.

In practice, that means that the schema building code has *two layers of
monads*: most functions in `Hasura.GraphQL.Schema.*`, that build parsers for
various GraphQL types, return the constructed parser in an outer monad `m` which
is an instance of `MonadSchema`; the parser itself operates in an inner monad
`n`, which is an instance of `MonadParse`.

## The GraphQL context

It's in `Hasura.GraphQL.Schema` that we build the actual "context" that is
stored in the [SchemaCache](#schema-cache): for each role we build the
`query_root` and the `mutation_root` (if any) by going over each source's table
cache and function cache. We do not have dedicated code for the subscription
root: we reuse the appropriate subset of the query root to build the
subscription root.
