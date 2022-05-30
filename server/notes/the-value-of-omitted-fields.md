This note is in [Hasura.GraphQL.Parser.Internal.Input](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Internal/Input.hs#L98).
It is referenced at:
  - line 1559 of [Hasura.GraphQL.Schema.Select](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Schema/Select.hs#L1559)

# The value of omitted fields


Suppose we had a table type like this:

    type article {
      comments(limit: Int!): [comment!]!
    }

Since the type of `limit` is non-nullable, and it does not have a default value, it is
/illegal/ to omit the argument. You’d /always/ have to provide some value---and
that isn't what we want, because the row limit should be optional. Hence, we
want to make the argument nullable:

    type article {
      comments(limit: Int): [comment!]!
    }

But this feels questionable. Should we really accept `null` values for `limit`?
That is, should this query be legal?

    {
      articles {
        comments(limit: null) { ... }
      }
    }

A tempting answer to that question is "yes": we can just treat a `null` value
for any optional field as precisely equivalent to leaving the field off
entirely. That is, any field with no default value really just has a default
value of `null`. Unfortunately, this approach turns out to be a really bad idea.
It's all too easy to write something like

    mutation delete_article_by_id($article_id: Int) {
      delete_articles(where: {id: {eq: $article_id}})
    }

then accidentally misspell `article_id` in the variables payload, and now you've
deleted all the articles in your database. Very bad.

So we'd really like to be able to have a way to say "this field is optional, but
`null` is not a legal value," but at first it seems like the GraphQL spec ties
our hands. Fortunately, there is a way out. The spec explicitly permits
distinguishing between the following two situations:

    comments { ... }
    comments(limit: null) { ... }

That is, the spec allows implementations to behave differently depending on
whether an argument was omitted or whether its value was `null`. This is spelled
out in a few different places in the spec, but §3.10 Input Objects
<http://spec.graphql.org/June2018/#sec-Input-Objects> is the most explicit:

> If the value `null` was provided for an input object field, and the field's
> type is not a non‐null type, an entry in the coerced unordered map is given
> the value `null`. In other words, there is a semantic difference between the
> explicitly provided value `null` versus having not provided a value.

Note that this is only allowed for fields that don't have any default value! If
the field were declared with an explicit `null` default value, like

    type article {
      comments(limit: Int = null): [comment!]!
    }

then it would not be legal to distinguish the two cases. Yes, this is all
terribly subtle.

Okay. So armed with that knowledge, what do we do about it? We offer three
different combinators for parsing input fields:

  1. `field` — Defines a field with no default value. The field's nullability is
       taken directly from the nullability of the field's value parser.
  2. `fieldOptional` — Defines a field with no default value that is always
       nullable. Returns Nothing if (and only if!) the field is omitted.
  3. `fieldWithDefault` — Defines a field with a default value.

The last of the three, `fieldWithDefault`, is actually the simplest. It
corresponds to a field with a default value, and the underlying value parser
will /always/ be called. If the field is omitted, the value parser is called
with the default value. (This makes it impossible to distinguish omitted fields
from those explicitly passed the default value, as mandated by the spec.) Use
`fieldWithDefault` for any field or argument with a non-`null` default value.

`field` is also fairly straightforward. It always calls its value parser, so if
the field is omitted, it calls it with a value of `null`. Notably, there is no
special handling for non-nullable fields, since the underlying parser will raise
an error in that case, anyway. Use `field` for required fields, and combine
`field` with `nullable` for optional fields with a default value of `null`.

`fieldOptional` is the most interesting. Unlike `field` and `fieldWithDefault`,
`fieldOptional` does not call its underlying value parser if the field is not
provided; it simply returns Nothing. If a value /is/ provided, it is passed
along without modification. This yields an interesting interaction when the
value parser does not actually accept nulls, such as a parser like this:

    fieldOptional $$(litName "limit") Nothing int

This corresponds to the `limit` field from our original example. If the field is
omitted, the `int` parser is not called, and the field parser just returns
Nothing. But if a value of `null` is explicitly provided, it is forwarded to the
`int` parser, which then rejects it with a parse error, since it does not accept
nulls. This is exactly the behavior we want.

This semantics can appear confusing. We end up with a field with a nullable type
for which `null` is not a legal value! A strange interpretation of “nullable”,
indeed. But realize that the nullability really means “optional”, and the
behavior makes more sense.

As a final point, note that similar behavior can be obtained with
`fieldWithDefault`. The following creates a boolean field that defaults to
`false` and rejects `null` values:

    fieldWithDefault $$(litName "includeDeprecated") Nothing (VBoolean False) boolean

This is a perfectly reasonable thing to do for exactly the same rationale behind
the use of `fieldOptional` above.
