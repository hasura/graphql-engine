# JSON database to GraphQL

[![oclif](https://img.shields.io/badge/cli-oclif-brightgreen.svg)](https://oclif.io)
[![Version](https://img.shields.io/npm/v/json2graphql.svg)](https://npmjs.org/package/json2graphql)

A CLI tool to import [JSON data](#json-structure) into Postgres and get a GraphQL API.

The GraphQL API is provided by [Hasura](https://github.com/hasura/graphql-engine) and `json2graphql` imports data into Postgres and applies Hasura configuration so that you can move from your sample JSON data to a postgres-backed GraphQL API instantly.

`json2graphql` is especially useful for using an existing JSON dataset to bootstrap a GraphQL API. Try some from [mongodb-json-files](https://github.com/ozlerhakan/mongodb-json-files). You might have to wrangle the dataset a teeny bit as per [this json-structure](#json-structure).


------------------------------------------

- [Quickstart](#quickstart)
- [Installation](#installation)
- [CLI Usage](#cli-usage)
- [JSON Structure](#json-structure)
- [Credits and related projects](#credits-and-related-projects)

## Demo

![demo-gif](https://graphql-engine-cdn.hasura.io/assets/json2graphql/j2g.gif)

In the above GIF, we are importing a schema and data from a JSON database. The Hasura GraphQL Engine is running at `https://j2gtest.herokuapp.com`

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

![json2graphql - From JSON to GraphQL on Postgres](https://graphql-engine-cdn.hasura.io/assets/json2graphql/json2postgres-graphql.png)

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

### Examples

For more examples, check out the [./example-datasets](./example-datasets) directory.

## Credits and related projects

- [Blowson](https://www.blowson.com/docs/) and its creator [Fredi Bach](https://fredibach.ch)
- [Firebase2GraphQL](https://firebase2graphql.com/): A tool to import data from firebase to a realtime GraphQL API on Postgres
- [json-graphql-server](https://github.com/marmelab/json-graphql-server)

