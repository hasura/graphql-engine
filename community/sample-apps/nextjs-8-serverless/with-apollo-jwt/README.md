# nextjs-8-jwt

> Boilerplate to get started with Next.js 8 Serverless Mode and JWT Authentication, Hasura GraphQL engine as CMS and postgres as database. 

## Deploy Hasura
- Deploy Postgres and GraphQL Engine on Heroku:
  
  [![Deploy to heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

  Please checkout our [docs](https://docs.hasura.io/1.0/graphql/manual/deployment/index.html) for other deployment methods

- Get the Heroku app URL (say `my-app.herokuapp.com`)

### Create the initial tables
1. Add your heroku URL in `hasura/config.yaml`

```yaml
endpoint: https://<hge-heroku-url>
```

2. Run `hasura migrate apply` and `hasura metadata apply` inside `hasura` directory to create the required tables and permissions for the app.

## Deploy JWT Server

- Deploy the JWT server by following the instructions [here](https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/auth-servers/passportjs-jwt-roles)
*Note* Skip the `knex migrate` step as the required tables have already been created in the above step.

Ensure to configure Hasura GraphQL Engine with the environment variables `HASURA_GRAPHQL_ADMIN_SECRET` and `HASURA_GRAPHQL_JWT_SECRET` as given in the above repo.

## Run the Next.js App

- Clone this repo:
  ```bash
  git clone https://github.com/hasura/graphql-engine
  cd graphql-engine/community/sample-apps/nextjs-8-serverless/with-apollo-jwt
  ```

- Install npm modules:
  ```bash
  cd app 
  yarn install
  ```

- Open `lib/init-apollo.js` and configure Hasura's GraphQL Endpoint as follows:

  ```js
  const httpLink = new HttpLink({
      uri: 'https://myapp.herokuapp.com/v1/graphql', // Server URL (must be absolute)
      credentials: 'same-origin' // Additional fetch() options like `credentials` or `headers`
  })
  ```
Replace the `uri` with your own Hasura GraphQL endpoint.

We are using an Auth Middleware to inject the Authorization headers into the GraphQL requests.

```js
  const authMiddleware = new ApolloLink((operation, forward) => {
    // add the authorization to the headers
    operation.setContext({
      headers: {
        authorization: getToken(),
      }
  })
```

Let's also configure the JWT server URL used for authentication.

Open `server.js` and update the following appropriately.
```
const target = 'http://localhost:8080' 
```

In this example, we integrate Apollo with Next by wrapping our *pages/_app.js* inside a higher-order component HOC. Using the HOC pattern we're able to pass down a central store of query result data created by Apollo into our React component hierarchy defined inside each page of our Next application.

On initial page load, while on the server and inside `getInitialProps`, we invoke the Apollo method,  [`getDataFromTree`](https://www.apollographql.com/docs/react/features/server-side-rendering.html#getDataFromTree). This method returns a promise; at the point in which the promise resolves, our Apollo Client store is completely initialized.

- We have defined the graphql query in `components/ArticleList.js`. 

    ```graphql
    query {
      article {
        id
        title
      }
    }
    ```

- Run the app:
  ```bash
  yarn run build
  yarn run dev
  ```
- Test the app
  Visit [http://localhost:3000](http://localhost:3000) to view the app

## Serverless Mode

With Next.js 8, each page in the `pages` directory becomes a serverless lambda. To enable `serverless` mode, we add the `serverless` build `target` in `next.config.js`.

```
module.exports = {
  target: "serverless",
};
```

That's it! Now build the serverless app by running the following command:

```
yarn run build
```

In the `.next` folder, you will see a `serverless` folder generated after the build. Inside that there is a `pages` folder, which will have outputs of lambda per page.

```
pages/index.js => .next/serverless/pages/index.js
pages/login.js => .next/serverless/pages/login.js
pages/signup.js => .next/serverless/pages/signup.js
pages/articles.js => .next/serverless/pages/articles.js
```

## Deploy to now.sh

Deploy it to the cloud with [now](https://zeit.co/now) ([download](https://zeit.co/download)):

```bash
npm install -g now
now
```
*Note*: Older versions of now-cli doesn't support serverless mode.
Once the deployment is successful, you will be able to navigate to pages `/` and `/login`, `/signup` with each one internally being a lambda function which `now` manages.
