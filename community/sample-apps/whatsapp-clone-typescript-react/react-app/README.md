# WhatsApp Clone React Client

### Run instructions

Make sure to setup Hasura GraphQL Engine first.

#### Install Dependencies

    yarn install

Run codegen to generate TypeScript types

    yarn generate

**Note**: The types are generated from the server! So if you have `admin secret` enabled in your graphql-engine server, make sure to update the headers in `codegen.js` file.

Set environment variables. Open `.env` file and add the following env

```bash
REACT_APP_SERVER_URL='<graphql_engine_server_url'>
REACT_APP_AUTH_URL='<auth_server_url'>
REACT_APP_ENV='dev'
```

#### Start the app

```
yarn start
```

Note that the auth server should run on port `8010`. If you decide to change that, be sure to edit the `.env` file in the auth server.

### License

MIT

