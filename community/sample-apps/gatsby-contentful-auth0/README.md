# gatsby-contentful-auth0
This is the sample music playlist application demonstrating the Gatsby + Contentful Remote Join with Hasura GraphQL.

## Getting started

If you've cloned this repository, navigate into the directory and install the npm modules using this command:

```bash
npm install
```

> Note: if you clone this project through the Gatsby CLI, it will install the modules for you.

## Auth0
This application uses Auth0 to manage identity. Refer to the [Auth0 integration guide](https://docs.hasura.io/1.0/graphql/manual/guides/integrations/auth0-jwt.html) for the configuration.

### Modify auth config

Rename `.env.EXAMPLE` to `.env.development` (or `.env.production`) and replace `<value>` for `AUTH0_DOMAIN` and `AUTH0_CLIENTID` with your Auth0 domain prefix and your client ID. These can be found on your [client dashboard](https://manage.auth0.com/#/clients).

Replace the `<value>` for `AUTH0_CALLBACK` with the URL for your callback route. The default for development is `http://localhost:8000/callback`.

## Contentful
Contentful remote schema is added as part of the migration. Configure the environment variables in Hasura GraphQL Engine server for the types to get merged.

- `CONTENTFUL_API_KEY` 
- `CONTENTFUL_API_ENDPOINT` which is of the format https://graphql.contentful.com/content/v1/spaces/<space-id>

## Migrations

Execute the following command inside `hasura` to apply the migrations

```bash
hasura migrate apply
```

This will create all the necessary tables, relationships and remote joins.

## Run the app
You can start the development server with the following command:

```bash
gatsby develop
```

The app runs at `localhost:8000` by default.

