# json2graphql: From a JSON file to postgres-backed GraphQL

[![oclif](https://img.shields.io/badge/cli-oclif-brightgreen.svg)](https://oclif.io)
[![Version](https://img.shields.io/npm/v/json2graphql.svg)](https://npmjs.org/package/json2graphql)

`json2graphql` is a tool that imports [a JSON file](#json-structure) to initialise schema and data in Postgres and then allows you to start querying it with GraphQL.

![json2graphql - From JSON to GraphQL on Postgres](https://graphql-engine-cdn.hasura.io/assets/json2graphql/json2postgres-graphql.png)

[Hasura](https://hasura.io) is used to expose a realtime GraphQL API on Postgres. Once your schema and data is imported, you can instantly start running powerful queries with filters, pagination, sorting, fetching relations, insert/update/delete mutations and subscriptions too.

**Use-cases**:

- **Bootstrapping a GraphQL backend**: Try out this example of initialising a GraphQL chat backend using a messages/groups/users chat JSON file. [Try it out](#quickly-bootstrap-a-graphql-backend)
- **Play with a mongo dataset in Postgres & GraphQL**: Export a mongo JSON dump, import it to Postgres and start querying it with GraphQL. [Try it out](#play-with-graphql-on-your-mongodb-data)
- **Query existing JSON datasets over GraphQL**: Pick up a JSON dataset, import to your new or existing Hasura/Postgres instance and start querying it. Try using [jdorfman/awesome-json-datasets](https://github.com/jdorfman/awesome-json-datasets).

------------------------------------------

## Demo

![demo-gif](https://graphql-engine-cdn.hasura.io/assets/json2graphql/j2g.gif)

In the GIF above, we are importing a schema and data from a JSON database. The Hasura GraphQL Engine is running at `https://j2gtest.herokuapp.com`

------------------------------------------

- [Quickstart](#quickstart)
- [Installation](#installation)
- [CLI Usage](#cli-usage)
- [JSON Structure](#json-structure)
- [Use Cases](#use-cases)
- [Credits and related projects](#credits-and-related-projects)

## Quickstart

1. **Create a JSON file** Create a JSON file, say, `db.json` as:

   ```json
   {
       "post": [
           { "id": 1, "title": "Lorem Ipsum", "views": 254, "user_id": 123 },
           { "id": 2, "title": "Sic Dolor amet", "views": 65, "user_id": 456 }
       ],
       "user": [
           { "id": 123, "name": "John Doe" },
           { "id": 456, "name": "Alison Craus" }
       ],
       "comment": [
           { "id": 987, "post_id": 1, "body": "Consectetur adipiscing elit", "user_id": 123 },
           { "id": 995, "post_id": 2, "body": "Nam molestie pellentesque dui", "user_id": 456 },
           { "id": 999, "post_id": 1, "body": "quid agis", "user_id": 456 }
       ]
   }
   ```

2. **Run Hasura + Postgres**: Run the Hasura GraphQL Engine and Postgres on Heroku's free tier by clicking this button:

   [![Deploy to heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

   Note the URL. It will be of the form: `https://<app-name>.herokuapp.com`. Let's say it's `j2gtest.herokuapp.com`.
   For instructions on how to deploy Hasura in other environments, head to the [docs](https://docs.hasura.io/1.0/graphql/manual/getting-started/index.html).


3. **json2graphql**: We import schema, data and create Hasura configuration in one command:

    ```bash
    npm install -g json2graphql
    json2graphql https://<app-name>.herokuapp.com --db=./path/to/db.json 
    ```

4. **Run GraphQL queries**: You can query the data in Postgres tables over GraphQL using Hasura GraphQL Engine. You can make complicated queries like:

   ```graphql
   query {
     user {
       postsByUserId {
         id
         title
         commentsByPostId {
           body
           id
         }
       }
       id
     }
   }
   ```

5. **Behind the scenes**: The following schema is created in Postgres::

   ```sql

   user (
     id integer not null primary key,
     name text
   )

   post (
     id integer not null primary key,
     title text,
     views integer,
     user_id integer foreign key references user(id)
   )

   comment (
     id integer not null primary key,
     body text,
     post_id integer foreign key references post(id),
     user_id integer foreign key references user(id)
   )

   ```

## Installation

```bash
## Install globally
npm install -g json2graphql

## Or run as a one-off command
npx json2graphql <hasura-url> -d ./path/to/db.json
```

## CLI Usage

```bash
# Running against a hasura without an access key
json2graphql https://j2gtest.herokuapp.com -d ./path/to/db.json

# Running against a hasura with an access key
json2graphql https://j2gtest.herokuapp.com -k <access-key> -d ./path/to/db.json

# Reset configuration, schema & data and import
# Useful for updating schema structure or working against an existing Hasura setup
# WARNING: This will remove all existing schema/data before applying
json2graphql https://j2gtest.herokuapp.com --overwrite -d ./path/to/db.json
```

#### Command

```bash
json2graphql URL [flags]
```

#### Args

* `URL`: The URL where Hasura GraphQL Engine is running

#### Options

- `-d --db`: path to the JS file that exports your sample JSON database
- `-o --overwrite`: DANGER: Overwrite tables if they already exist in database
- `-v --version`: show CLI version
- `-h, --help`: show CLI help

## JSON structure

The top level of your JSON database should be a JSON object with keys being the name of entities and values being list of entities. For example:

```json
{
    "user": [
        { "id": 123, "name": "John Doe" },
        { "id": 456, "name": "Jane Doe" }
    ],
    "city": [
        { "id": 987, "name": "Stockholm", "country": "Sweden" },
        { "id": 995, "name": "Sydney", "country": "Australia" }
    ]
}
```

1. The JSON structure is a "normalised" set of objects
2. Top level objects are mapped to tables in postgres and root fields in the GraphQL schema
3. Keys in the objects are mapped to columns of the tables in postgres, and as fields in the GraphQL schema
4. Keys in the object with the column name of the form `<ENTITY_NAME>_id`, are considered to indicate foreign-key constraints on postgres, and connections in the GraphQL schema
5. The types of the columns/fields are inferred from the data in the columns
json2graphql treats top-level objects as tables, and their keys as columns. If it encounters a column name of the form `<ENTITY_NAME>_id`, json2graphql will consider it a foreign key to the entity with name `<ENTITY_NAME>`.

| JavaScript type (constructor.name) | Postgres column type         | GraphQL field type | Example data |
| ---------------------------------- | ---------------------------- | ------------------ | ------------ |
| Number | numeric | numeric | `12.34` or `1223` |
| String | text | String | `Hello world` |
| Boolean | bool | Boolean | true                     |
| Date | timestamptz | timestamptz | `new Date("Jan 24, 2010 00:00:00")` |
| Object or Array | jsonb | jsonb | { ... } |

### Generating data - importing with `.js` files

You can also use Javascript `.js` files. This allows you to:
- Write some generation logic for sample data
- Use `date` types

```js
module.exports = {
    user: [1,2,3,4,5].map(i => ({
      id: i,
      name: `user-${i}`,
      created: new Date()
    }))
};
```

## Use cases

### Play with GraphQL on your MongoDB data

**Note:** This assumes that you've already run through the [quickstart](#quickstart)!

You can migrate your data from MongoDB and explore Realtime GraphQL over it.

1. Tweak the MongoDB doc to fit the required [JSON structure](#json-structure).
2. Use json2graphql to import the data from the JSON
3. Make realtime GraphQL queries

Consider [this MongoDB doc](https://github.com/ozlerhakan/mongodb-json-files/blob/master/datasets/country.json):

1. Tweak the doc to fit the required JSON structure.

    The doc originally looks something like this:

    ```js
    {"_id":{"$oid":"55a0f42f20a4d760b5fc305e"},"altSpellings":["AI"],"area":91, ... }
    {"_id":{"$oid":"55a0f42f20a4d760b5fc305e"},"altSpellings":["AI"],"area":91, ... }
    {"_id":{"$oid":"55a0f42f20a4d760b5fc305e"},"altSpellings":["AI"],"area":91, ... }
    .
    .
    .
    ```

    You should wrap it in an array and make the array a value of a top level key of your choice, say, `country`. You should also field name `_id` to `id` because the CLI expects an `id` field. It should look something like this:

    ```js
    {
      "country": [
        {"id":{"$oid":"55a0f42f20a4d760b5fc305e"},"altSpellings":["AI"],"area":91, ... }
        {"id":{"$oid":"55a0f42f20a4d760b5fc305e"},"altSpellings":["AI"],"area":91, ... }
        {"id":{"$oid":"55a0f42f20a4d760b5fc305e"},"altSpellings":["AI"],"area":91, ... }
        .
        .
        .
      ]
    }
    ```

2. Use json2graphql to import the data from the JSON to Postgres using Hasura GraphQL Engine:

    ```
    json2graphql https://j2gtest.herokuapp.com -d ./db.js
    ```

3. Try realtime GraphQL. Go to your GraphQL Engine console and try making GraphQL queries like so:

    ```gql
    query {
      country (
        order_by: { name: asc }
        limit: 10
        where: { capital: { _is_null: false }}
      ){
        id
        name
        area
        currency
        callingCode
        capital
      }
    }
    ```

### Quickly bootstrap a GraphQL Backend

**Note:** This assumes that you've already run through the [quickstart](#quickstart)!

You can write your schema and data in JSON format to quickly get a Realtime GraphQL API.

For example, to start with a group chat backend:

```
{
  "user": [
    { "id": 1, "name": "John Doe", "username": "johndoe", "last_seen": new Date() },
    { "id": 2, "name": "Alice Wan", "username": "alisson", "last_seen": new Date() },
    { "id": 3, "name": "Natalie Jackson", "username": "nats", "last_seen": new Date() },
    { "id": 4, "name": "George Walsh", "username": "georgee", "last_seen": new Date() }
  ],
  "group": [
    { "id": 1, "name": "Engineering", is_active: true },
    { "id": 2, "name": "Marketting", is_active: false }
  ],
  "message": [
    { "id": 1, group_id: 1, "body": "Message 1", "sent_at": new Date(), "user_id": 1 },
    { "id": 2, group_id: 1, "body": "Message 2", "sent_at": new Date(), "user_id": 2 },
    { "id": 3, group_id: 2, "body": "Message 3", "sent_at": new Date(), "user_id": 3 },
    { "id": 4, group_id: 2, "body": "Message 4", "sent_at": new Date(), "user_id": 4 }
  ]
}
```

You can import the above JSON dataset and make queries like:

```gql
# fetch all the active groups
query fetch_groups {
  group (
    where: {is_active: { _eq: true }}
    order_by: { name: asc }
  ){
    id
    is_active
    name
  }
}

# fetch all messages from a group
query fetch_messeges_from_a_group {
  message(
    where: { group_id: { _eq: 1 }}
    order_by: { sent_at: asc }
  ) {
    id
    body
    sent_at
    sent_by: userByUserId {
      id
      username
    }
  }
}
```

## Examples

For more examples, check out the [./example-datasets](./example-datasets) directory.

## Credits and related projects

- [Blowson](https://www.blowson.com/docs/) and its creator [Fredi Bach](https://fredibach.ch)
- [firebase2graphql](https://firebase2graphql.com/): A tool to import data from firebase to a realtime GraphQL API on Postgres
- [json-graphql-server](https://github.com/marmelab/json-graphql-server)
