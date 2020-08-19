# Pagination demo with React, Relay, and Hasura

This app demonstrates pagination using Hasura with Relay. It's for demo purposes only, and not a complete boilerplate app.

## Setup

- Create the following tables via the Hasura console:
  - `reviews`
    - columns: `id`, `body`, `created_at`, `restaurant_id`
  - `restaurants`
    - columns: `id`, `name`, `cuisine`
- Create a [one-to-many relationship](https://hasura.io/docs/1.0/graphql/manual/schema/relationships/database-modelling/one-to-many.html) between the tables.
- Using the Hasura console, add some rows to both tables. Add at least four reviews since the sample code loads three reviews at a time.
- Using your Relay endpoint from Hasura (`/v1beta1/relay`), export your GraphQL schema by following the instructions [here](https://hasura.io/docs/1.0/graphql/manual/schema/export-graphql-schema.html) (to replace `schema.graphql` at the root of this project).
- In `fetchGraphQL.js`, set the GraphQL endpoint to your Relay endpoint.
- In `App.js`, replace `MY_RESTAURANT_ID` with a restaurant `id` from your database (This is for demo purposes; normally you'd pass in the `id` via routing).
- In your Terminal, at the root of the app:
  - Run `yarn install`.
  - Run the Relay complier with `yarn run relay --watch`.
  - In a separate tab, run the React app with `yarn start`.
- Open [http://localhost:3000](http://localhost:3000) to view the app in your browser.
- Click the `Load More` button to load more reviews.

For more information on Hasura's Relay API, see the [Hasura docs](https://hasura.io/docs/1.0/graphql/manual/schema/relay-schema.html).
