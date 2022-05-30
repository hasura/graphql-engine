This note is in [Hasura.GraphQL.Schema.Remote](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Schema/Remote.hs#L127).
It is referenced at:
  - line 246 of [Hasura.GraphQL.Schema.Remote](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Schema/Remote.hs#L246)
  - line 352 of [Hasura.GraphQL.Schema.Remote](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Schema/Remote.hs#L352)
  - line 451 of [Hasura.GraphQL.Schema.Remote](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Schema/Remote.hs#L451)

# Variable expansion in remote schema input parsers

### Input parsers as lightweight type checkers

The purpose of input parsers for remote schemas is not to translate the provided input values into
an internal representation: those values will be transmitted more or less unmodified to the remote
service; their main purpose is simply to check the shape of the input against the remote schema.

Consider, for instance, the following remote schema:

    input Foo {
      bar: Int!
    }

    type Query {
      run(foo: Foo!): Int!
    }

Our parsers will need to decide which invocations of `run` are valid:

    query {
      run(null)             # invalid: foo is non-nullable
      run(foo: {baz: 0})    # invalid: Foo doesn't have a "baz" field
      run(foo: {bar: "0"})  # actually valid!
    }

That last example is surprising: why would we accept a string literal for an Int? It simply is
because we delegate the task of translating the literal into a scalar to the remote server. After
all, *we* advertise some values as Int in the schema, despite accepting string literals.

### Inserting remote permissions presets

Where things get more complicated is with remote permissions. We allow users to specify "presets":
values that will always be provided to the remote schema, and that the user cannot customize in
their query. For instance, given the following schema with permissions:

    input Range {
      low:  Int! @preset(value: 0)
      high: Int!
    }

    type Query {
      getValues(range: Range!): [Int]
    }

a user cannot specify "low" in OUR schema, as we will insert its value when parsing the incoming
query. This is the second purpose of those input parsers: they insert remote schema presets where
required. In this case:

    # we receive
    query {
      getValues(range: {high: 42})
    }

    # we emit
    query {
      getValues(range: {low: 0, high: 42})
    }

### Variable expansion

But where this gets even more complicated is with variables. As much as possible, we simply forward
variables without interpeting them (not all JSON values are representable in GraphQL). We do so
whenever possible; for instance, using the previously established remote schema:

    # we receive
    query:
      query($h: Int!) {
        getValues(range: {high: $h})
      }
    variables:
      { "h": 42 }

    # we emit
    query:
      query($h: Int!) {
        getValues(range: {low: 0, high: $h})
      }
    variables:
      { "h": 42 }

The tricky case is when a preset field is *within a variable*. We then have no choice: we have to
expand the variable, and rewrap the value as best as we can, to minimize the amount of JSON
evaluation. For instance:

    # we receive
    query:
      query($r: Range!) {
        getValues(range: $r)
      }
    variables:
      { "r": {"high": 42} }

    # we emit
    query:
      query($hasura_json_var_1: Int!) {
        getValues(range: {low: 0, high: $hasura_json_var_1})
      }
    variables:
      { "hasura_json_var_1": 42 }

Our parsers, like all others in our model, expand the variables as they traverse the tree, and add
the preset values where required. But the downside of this is that we will create one such JSON
variable per scalar within a JSON variable!

### Short-circuiting optimization

To avoid this, we track in the parsers whether an alteration has occured: if we had to insert a
preset value. As long as we don't, we can discard the output of the parser, as it will contain the
exact same value as the input (if perhaps represented differently); by discarding the output and
just forwarding the input, we avoid expanding variables if no preset needs be inserted.

