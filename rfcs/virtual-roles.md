# Virtual roles

## Motivation

Hasura has a very flexible role based permission system. But flexibility also comes with the cost of complexity and higher maintenance cost.
A role can have many purposes and usually those purposes are declared as permission rules, for example the author of a post can edit the post, and the topic admin can edit all post
in a given topic.

The permission for the user role would look like this:

```json
{
  "_or": [
    { "author_id": { "_eq": "x-hasura-user-id" } },
    { "topic": { "admin_id": { "_eq": "x-hasura-user-id" } } },
  ]
}
```

As a result the permission page would show something like this in table

| role | ... | update |
|---|---|---|
| ... | ... | ... |
| user | ... | Y (funnel) |

Although the permissions in this example are choosen to be very basic, to understand the permission for the `user` role, one have to know what the fields (author_id & topic.admin_id) do mean.

But the main issue here is, that we actually have two roles, _virtual roles_.

1. `author`
2. `topic admin` 

What if we could create those roles directly in the console?

## Implementation idea from the UI perspective

### 1. Create virtual roles

On the permissions page the user sees a **add virtual role** Button.
1. On click a form opens
2. Fields of the form are `name`, `parent-role (select list)` & `json rule editor` and a Save button

To create the role `author` we would only need to write and save:

* name: `author`
* parent-role: `user`
* rule: `{ author_id: { _eq: "x-hasura-user-id"}}`

For the topic admin

* name: `topic admin`
* parent-role: `user`
* rule: `{ topic: { admin_id: { _eq: "x-hasura-user-id" } } }`

### Configure virtual rules

| role | update |
| ---  | ---------- |
| user   | X |
| _author_   | √ |
| _topic admin_   | √ |

Note: virtual roles are in _italic_ style  
The configuration of columns and other stuff, even more rules should be possible like for any other role

## Using

Becuase `virtual roles` are very close to the actual business logic, the claim should be passed explicitly as a param in the query itself.
For example if we want to change the title of the post **as** the author of a post we could have an api like this:

```
mutation changePostTitle($id: uuid!, $title: text!) {
  update_post_by_pk(id: $id, _as: "author", _set: { title: $title }) {
    id
    title
  }
}
```

## Further thoughts

Because virtual roles are basically `role` + `rules on fields`. They do not depend on table names, which could allow us to create global `virtual roles` like `owner` where the rule is basically `{ user_id: { _eq: "x-hasura-user-id" }}` and reuse them in all of our tables.
