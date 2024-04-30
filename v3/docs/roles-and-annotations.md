# Roles / namespaces / annotations in `v3-engine`

In V2 we created a schema per role.

In V3 we create one big schema at build time, where some nodes are annotated
with either `generic` info, or `namespaced` info that only applies to specific
roles.

The types for these annotations come from the `lang-graphql` crate, where they
are associated types on the `SchemaContext` trait.

This example used in tests for `lang-graphql` shows a schema with no
annotations. It uses a `Namespace` type to differentiate different scopes,
however since it has no annotations this won't do anything meaningful:

```rust
impl SchemaContext for SDL {
    type Namespace = Namespace;
    type GenericNodeInfo = ();
    type NamespacedNodeInfo = ();
```

The `engine` itself uses the `GDS` type, and so we can use the following
annotations:

```rust
impl gql_schema::SchemaContext for GDS {
    type Namespace = Role;
    type GenericNodeInfo = types::Annotation;
    type NamespacedNodeInfo = Option<types::NamespaceAnnotation>;
```

Note that our `Namespace` type is `Role` - this means that the engine attached
useful information per role, so we can add select permissions for a field that
only apply to the `user-1` role, for instance.

(incidentally, there is a comment on the `SchemaContext` trait suggesting
`Namespace` is renamed to `Scope` or `Role` - this seems like a useful move to
make all this clearer, personally `Namespace` makes me think about different
subgraphs or something)

## What is an annotation then, concretely?

The change I have recently been working on is to add preset arguments to
commands. This means that given a `delete_user` command with a `user_id: Int`
argument, we can preset the value for certain roles.

For instance, we might want a `user-1` role to only be able to delete
themselves, so we'd preset `user_id` with
`{ "sessionVariable": "x-hasura-user-id" }`.

Previous to this change, when building the schema for the Command, we'd use
`builder.allow_all_namespaced` to create it, like

```rust
builder.allow_all_namespace(command_schema_stuff, None)
```

Instead, for our example, we'd use `conditional_namespaced` like this (excuse my
pseudo-Rust):

```rust
let role_annotations = HashMap::new();

role_annotations.insert("user-1", Some(ArgumentPresets { "user_id":
"x-hasura-role-id" }));

role_annotations.insert("admin", None);

builder.conditional_namespaced(command_schema_stuff, role_annotations)
```

Here, we've added an annotation for `user-1` with some useful information we can
use later when receiving and running queries. We've also added a `None`
annotation for `admin` - this means there'll be no useful information later, but
we're still signalling that we want this `Command` to work for `admin` users. If
we had a `user-2` role in the schema, they wouldn't be able to use the command.

## Removing items from the GraphQL schema, per role

By adding or omitting keys from the `role_annotations` above, we've made our
command appear or disappear from the GraphQL schema. This "snipping" as such,
happens in the `normalize_request` function in `lang-graphql`.

The important thing to know here is that `lang-graphql` code does not know about
`GDS` or our `NamespaceAnnotation` type. All it has to act on is whether a
`Role` (or `Namespace`, to it's eyes) has a key in any namespaced annotations or
not. Because we're using associated types the contents are "protected" from
`lang-graphql` and so it can't peek inside.

We're going to want the `user_id` argument to disappear from `user-1`'s schema

- we do this by using `conditional_namespaced` when contructing schema for the
  `user_id` command argument itself:

```rust
let role_annotations = HashMap::new();

// insert an empty annotation for `admin` to make sure the argument remains in
the schema
role_annotations.insert("admin", None);

// don't add one for `user-1`, so it disappears from the schema (as it has been
replaced with the preset value)
builder.conditional_namespaced(command_argument_schema_stuff, role_annotations)
```

## Reading the annotations at request time

Everything before here happens at "compile time" for the engine, so we do it all
once at startup and it remains static for the lifetime of the application.

At some point we are going to need to
[serve a request](https://github.com/hasura/v3-engine/blob/main/engine/src/execute.rs#L170)
though. The steps are as follows:

- Receive request
- Parse into a GraphQL query
- Normalize the query
- Generate IR
- Construct a query plan
- Execute / explain the query plan

The `normalize` step was mentioned earlier - it takes place in `lang_graphql`
and combines the query with the role information to remove all irrelevant
namespace information from the query, and snip any parts of the schema that the
current role can't see.

Generally the helpful place for our annotations is in generating IR
(intermediate representation, compiler fans). For our commands change, we can
look up the `ArgumentPresets` we stored earlier like
[this](https://github.com/hasura/v3-engine/pull/340/files#diff-f01744b02938317df22c7bc991717ae20a397f623c387332f103a30d1c0d2dc9R104).

```rust
match field_call.info.namespaced {
  None => {}
  Some(NamespaceAnnotation::ArgumentPresets(argument_presets)) => {
    // use `argument_presets` for current role to generate IR
    ...
  }
}
```

The IR will then be created from a mixture of the schema for the user's role,
and any arguments etc from their request.
