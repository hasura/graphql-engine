# Angular-Hasura-Boilerplate - hello-world

## Getting Started

### Installing

You need to first install all the npm packages that are used in this boilerplate.

```
$ npm install
```

Once all the dependencies are installed, you're ready to go!

```
$ npm start
```

This starts the development server at localhost:4200.

### What does this boilerplate contain ?

This is a simple and easy to use boilerplate which gives you start by doing all the work connecting your hasura graphql endpoint using `ApolloClient` .

In the `graph-ql.module.ts` the file initializes the basic settings for the Apollo client which will be used to communicate with your Hasura Endpoint.

```
    const uri = environment.graphqlEndpoint;

    /** Following values need to be added to the header before making any
     *  query.
     *  1. X-Hasura-Access-Key: This Access Key is what will let your app access your endpoint.
     *  2. Content-Type: To tell the type of content.
     *  3. Authorization: This is the token that tells that a user is logged in.
     *  4. X-Hasura-Role: This will be 'user' to let Hasura know that a user is accessing the endpoint.
     *  5. X-Hasura-User-Id: This the user id of the user.
     */
    const authHeader = new HttpHeaders()
      .set("X-Hasura-Access-Key", environment.hasuraAccessKey)
      .set("Content-Type", "application/json")
      .set("Authorization", `Bearer <token-goes-here>`)
      .set("X-Hasura-Role", "user")
      .set("X-Hasura-User-Id", "<user_id>");

    // Create a HTTP Link with the URI and the header.
    const http = httpLink.create({ uri, headers: authHeader });

    // Create an Apollo client with HTTP Link and cache as InMemoryCache.
    apollo.create({
      link: http,
      cache: new InMemoryCache()
    });
```
