## GraphiQL

This version of GraphiQL is a fork of the original version with a simple header management UI.

You can access it live here - https://learn.hasura.io/graphql/graphiql

## Usage of Environment Variables

This app uses a few environment variables which are required for development. The production build uses values directly present in index.html serving this app.

We use [dotenv](https://github.com/motdotla/dotenv) for setting environment variables for development. Create a `.env` file in the root directory (wherever package.json is) and set the following values. Replace accordingly for testing.

```
PORT=3000
NODE_ENV=development
GRAPHQL_ENDPOINT=http://localhost:8090/v1/graphql
HEADER_STRING='{}'
VARIABLE_STRING='{}'
QUERY_STRING='query { test_table { id } }'
REACT_APP_CALLBACK_URL='http://localhost:3000/callback'
```

**Note**
The .env file should not be in version control.

## Deployment

```
$ npm run build
```

The static assets will be generated in `static` folder. There is an index.html file referencing the css and js assets inside `dist` folder.