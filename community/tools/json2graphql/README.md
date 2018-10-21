# JSON database to GraphQL

[Hasura GraphQL Engine](https://hasura.io) gives instant GraphQL APIs over Postgres.

This is A CLI tool to import a schema and data to Postgres using JSON data. You can then leverage all the features of Hasura GraphQL Engine to query the Postgres data over GraphQL.

[![oclif](https://img.shields.io/badge/cli-oclif-brightgreen.svg)](https://oclif.io)
[![Version](https://img.shields.io/npm/v/json2graphql.svg)](https://npmjs.org/package/json2graphql)

## Quick start

1. Quickly get the GraphQL Engine running by clicking this button:

   [![Deploy to heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

  Note the URL. It will be of the form: `https://<app-name>.herokuapp.com`

2. Create a db.js file. Your data file should export an object where the keys are the entity types. The values should be lists of entities, i.e. arrays of value objects with at least an id key. For instance:

    ```js
    module.exports = {
        user: [
            { id: 123, name: "John Doe" },
            { id: 456, name: "Jane Doe" }
        ],
        city: [
            { id: 987, name: "Stockholm", country: "Sweden" },
            { id: 995, name: "Sydney", country: "Australia" }
        ]
    }
    ```

3. Use the CLI to import the data:

    ```
    $ npm install -g json2graphql
    $ json2graphql https://<app-name>.herokuapp.com --db=./path/to/db.js 
    ```

4. That's it. You can go to your HGE URL `https://<app-name>.herokuapp.com` and start querying this data over GraphQL:

    ```graphql
    query {
      user {
        id
        name
      }
      city {
        id
        name
        country
      }
    }
    ```

Check [this section](#foreign-keys-and-relationships) for knowing about foreign keys and relationships.

## Installation

### CLI

```bash
npm install -g json2graphql
```

## Usage

### CLI

#### Without access key

```
$ json2graphql https://hge.herokuapp.com -d ./path/to/db.js
```

#### With access key

```
$ json2graphql https://hge.herokuapp.com -k <access-key> -d ./path/to/db.js
```

### Command

```bash
$ gq URL [flags]
```

#### Args

* `URL`: The URL where Hasura GraphQL Engine is running

#### Options

- `-d --db`: path to the JS file that exports your sample JSON database
- `-o --overwrite`: Overwrite tables if they already exist in database
- `-v --version`: show CLI version
- `-h, --help`: show CLI help

## More features

### Foreign keys and relationships

You can also define foreign keys and relationships in your JSON sample data. The CLI infers foreign keys and relationships from column names and table names.

For example, in the following data set, the `posts` table has a field called `users_id` which is a foreign key to the `id`  column of table `users`. Also, the `comments` table has a field called `posts_id` which is a foreign key to the `id`  column of table `posts`.

```js
module.exports = {
    post: [
        { id: 1, title: "Lorem Ipsum", views: 254, user_id: 123 },
        { id: 2, title: "Sic Dolor amet", views: 65, user_id: 456 },
    ],
    user: [
        { id: 123, name: "John Doe" },
        { id: 456, name: "Jane Doe" }
    ],
    comment: [
        { id: 987, post_id: 1, body: "Consectetur adipiscing elit" },
        { id: 995, post_id: 1, body: "Nam molestie pellentesque dui" }
    ]
};
```

Import the database:

```
$ json2graphql https://<app-name>.herokuapp.com --db=./path/to/db.js
```

Now you can make complicated queries like:

```graphql
query {
  post {
    id
    title
    views
    userByUserId {
      id
      name
    }
    commentsByPostId {
      id
      body
    }
  }
}
```

The response would be:

```json
{
  "data": {
    "post": [
      {
        "userByUserId": {
          "name": "John Doe",
          "id": 123
        },
        "views": 254,
        "id": 1,
        "title": "Lorem Ipsum",
        "commentsByPostId": [
          {
            "body": "Consectetur adipiscing elit",
            "id": 987
          },
          {
            "body": "Nam molestie pellentesque dui",
            "id": 995
          }
        ]
      },
      {
        "userByUserId": {
          "name": "Jane Doe",
          "id": 456
        },
        "views": 65,
        "id": 2,
        "title": "Sic Dolor amet",
        "commentsByPostId": []
      }
    ]
  }
}
```

### Overwrite

If your Postgres already contains tables that you are trying to import using `json2graphql`, the command will fail.

If you want to overwrite the existing tables in the database with the new tables from your sample JSON database, you must provide a flag `-o` or `--overwrite`

---
Maintained with :heart: by <a href="https://hasura.io">Hasura</a>
