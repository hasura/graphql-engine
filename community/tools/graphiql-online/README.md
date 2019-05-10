## GraphiQL Demo

This version of GraphiQL is a fork of the original version with a simple header management UI.

You can access it live here - https://graphiql-online.com

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
```

**Note**
The .env file should not be in version control.

## Deployment

```
$ npm run build
```

The static assets will be generated in `static` folder. There is an index.html file referencing the css and js assets inside `dist` folder.

For a quick Docker based deployment, use `docker build -t graphiql .` && `docker run -d -p 8080:8080 graphiql` for running the production build locally.

You can also use now.sh for a cloud deployment. Just simply run `now` to deploy this and get a live URL.
