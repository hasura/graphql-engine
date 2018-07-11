# Hasura GraphQL Engine

[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=svg)](https://circleci.com/gh/hasura/graphql-engine)

Hasura GraphQL Engine is a performant GraphQL server that provides a **ready-to-use GraphQL API over Postgres** by auto-generating a GraphQL schema and corresponding resolvers. 

* Make powerful queries: built-in filtering, pagination and pattern search arguments and operators
* ??: add it to an existing, living Postgres database
* Fine-grained access control: dynamic access control that
integrates with your auth
* Lightweight & blazing fast: stats + link to Performance Benchmarks section
* Dev friendly: Admin UI & Rails-inspired schema migrations
* Postgres ❤️: supports Postgres types (PostGIS/geo-location, etc.), turns views to *graphs*, trigger stored functions or procedures with
mutations

## Quickstart

### Setup on Heroku (recommended)

:information_source: *no credit-card required*; Sign-up required

1. Click on the following button to deploy GraphQL Engine on Heroku and provision the free Postgres add-on:

    [![Deploy](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. Open the admin console

   Visit https://\<app-name\>.herokuapp.com (replace \<app-name\> with your app name) to open the admin console.

3. Make your first GraphQL query

   Create test tables and instantly run your first query. Follow this [simple guide] (https://docs.hasura.io/1.0/graphql/manual/getting-started/first-graphql-query.html).



### Setup using Docker

1. Install the Hasura CLI:

    * For Mac or Linux: run the following command in your terminal/shell<sup>*</sup>
      
      ```bash
      curl -L https://cli.hasura.io/install.sh | bash
      ```
      <sup>*</sup> *installs the CLI in `/usr/local/bin` (Mac) or `/usr/local/bin` (Linux). `sudo` password may be required.*

    * For Windows: Download relevant installer<sup>*</sup>
        * [64-bit Windows installer](https://cli.hasura.io/install/windows-amd64)
        * [32-bit Windows installer](https://cli.hasura.io/install/windows-386)

        <sup>*</sup> *run the installer as `Administrator`. CLI needs [git bash](https://git-scm.com/download/win). Restart `git bash` if you see a `command not found` error after installation.*

2. Initialize a project using CLI

   ```bash
      hasura init --directory my-project
   ```

3. Run Hasura & Postgres

   ```bash
      cd my-project/install-scripts
      docker-compose up -d
   ```

4. Open the admin console

    * Edit the `my-project/config.yaml` file to set the endpoint:
      
      ```yaml
      endpoint: http://localhost:8080
      ```

    * Open console:
      
      ```bash
      # Run this command in the my-project/ directory
      hasura console
      ```

5. Make your first GraphQL query

   Create test tables and instantly run your first query. Follow this [simple guide] (https://docs.hasura.io/1.0/graphql/manual/getting-started/first-graphql-query.html).


:information_source: See detailed deployment guides [here](https://docs.hasura.io/1.0/graphql/manual/getting-started/index.html)

[//]: # (Remove if we can add a link at the top)

## Documentation

[Hasura GraphQL Engine docs](https://docs.hasura.io)

## Examples

* GitLab schema
* ??

## Architecture

The Hasura GraphQL Engine fronts a Postgres database instance and can serve as an API Gateway for all data requests from client apps. It can be configured to work with your existing Auth middleware to handle access control to data using field-level rules with dynamic variables.

The GraphQL Engine is a standalone, multi-core aware component that can be scaled vertically and horizontally.**??**

![alt text](https://hasura.io/rstatic/dist/3021ad7d73fb15e8f7bdf86612ebd8a9.png "GraphQL Engine basic architecture")

You can also place the engine behind a central GrapQL proxy that fronts multiple GraphQL APIs via schema stitching.

![alt text](https://docs.platform.hasura.io/0.15/_images/graphql-schema-stitching.png "GraphQL Engine schema-stitched architecture")

## Client-side tooling

GraphQL Engine provides an http API. This means you can use any client-side tool or library to query the endpoint. To fully leverage GraphQL, it is recommended that you use GraphQL specific client-side tooling like [Apollo Client](https://github.com/apollographql/apollo-client), [Relay](https://github.com/facebook/relay), etc. See [awesome-graphql](https://github.com/chentsulin/awesome-graphql) for a list of such tools.

## Community

GraphQL Engine has a burgeoning community of amazing developers! Come join the community using your preferred means of communication:

- [Discord](https://discord.gg/vBPpJkS)
- [Intercom chat](https://hasura.io/help)
- [Twitter](https://twitter.com/hasurahq)
- [Email](mailto:build@hasura.io)
- [Facebook](https://www.facebook.com/HasuraHQ/)

Hasura follows the CNCF code of conduct....

## Contributing

Checkout our [contributing guide](CONTRIBUTING.md) for more details.

## License

GraphQL Engine is available under the [GNU Affero General Public License v3](https://www.gnu.org/licenses/agpl-3.0.en.html) (AGPL-3.0). [Why AGPL](https://gist.github.com/hasura-bot/9c36a0201a7563f7762b265a12b044d5).

:information_source: If you are not satisfied with this license, commercial licenses are available on request. Please feel free to contact us at build@hasura.io or [hasura.io/help](https://hasura.io/help).


