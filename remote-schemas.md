# Remote schemas

Merge remote GraphQL schemas with GraphQL Engine's Postgres-based schema to query all your GraphQL types from the same endpoint. Remote schemas are ideal for use cases such as:

* Customizing mutations (*e.g. running validations before inserts*)
* Supporting features like payments, etc. and providing a consistent interface to access them i.e. behind the GraphQL Engine's API
* Fetching disparate data from other sources (*e.g. from a weather API or another database*)

To support custom business logic, you'll need to create a custom GraphQL server (see [boilerplates](community/boilerplates/graphql-servers)) and merge its schema with GraphQL Engine's.

![remote schems architecture](assets/remote-schemas-arch.png)

## Demo (*40 seconds*)

[![video demo of merging remote schemas](https://img.youtube.com/vi/eY4n9aPsi0M/0.jpg)](https://www.youtube.com/watch?v=eY4n9aPsi0M)

[Merge remote GraphQL schemas (YouTube link)](https://youtu.be/eY4n9aPsi0M)

## Quickstart

The fastest way to try remote schema out is via Heroku.

1. Click on the following button to deploy GraphQL Engine on Heroku with the free Postgres add-on:

    [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. Open the Hasura console

   Visit `https://<app-name>.herokuapp.com` (*replace \<app-name\> with your app name*) to open the admin console.

3. Merge your first remote schema and query it

   In the admin console, open the ``Remote Schemas`` tab and click on the ``Add`` button. Fill in the following details:
   * Remote Schema name: ``countries`` (*an alias for this remote schema*).
   * GraphQL server URL: ``https://countries.trevorblades.com/`` (*a public GraphQL API that we'll use to quickly check out this feature; maintained by [@trevorblades](https://github.com/trevorblades)*. 
   * Ignore the remaining configuration settings and click on the ``Add Remote Schema`` button.

   Head to the ``GraphiQL` tab and run the following query (*paste it in the query window on the left and click the* ▶️ *(play) button*):

   ```graphql
   {
      countries {
        emoji
        name
        languages {
          name
          native
        }
      }
   }
   ```

   You can explore the GraphQL types from the remote schema using the ``Docs`` explorer in the top right corner of the ``GraphiQL`` interface.

## Boilerplates

Boilerplates for custom GraphQL servers in popular languages/frameworks are available.

* [Regular boilerplates](community/boilerplates/graphql-servers) that can be deployed anywhere.
* [Serverless boilerplates](https://github.com/hasura/graphql-serverless) that can deployed on serverless platforms like AWS Lambda, etc.

Please note that boilerplates for more languages, frameworks, serverless platforms, etc. are being iterated upon and community contributions are very welcome. 


## Caveats

In this first iteration, conflicting top level nodes are not supported.Please be careful with your custom schema's nomenclature and ensure that the top-level node names in it do not clash with the top-level nodes in GraphQL Engine's schema. Also, having remote GraphQL nodes and Hasura nodes in a single query/mutation will throw and error.

We plan to address these issues in the upcoming iterations.

## Documentation

Read the complete [documentation](https://docs.hasura.io/1.0/graphql/manual/remote-schemas/index.html).


