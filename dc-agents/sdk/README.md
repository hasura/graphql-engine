# Data Connector SDK

This SDK serves as a pack of documentation and resources for understanding, building, and testing Data Connector agent implementations to ensure that they are complete, correct, and idiomatic and can be developed rapidly and with confidence. The SDK is intended to evolve with time, gaining features and changing format to reflect the best practices for agent-development recommended by Hasura. As such, the SDK will be versioned and correlate with HGE versions via the included `.env` file.

In addition to the SDK archive, some information may be mirrored in the [docs.hasura](https://hasura.io/docs/latest/graphql/core/index/) documentation.


## What is a Data Connector (Agent)?

A Hasura GraphQL Engine Data Connector Agent is a service that abstracts a datasource behind a REST API and well-defined wire format. Further information can be found in `README_DATA_CONNECTORS.md` which describes the design and architecture of the feature. An agent facilitates the extension of database support for Hasura GraphQL Engine by allowing configuration of Agents at runtime through the HGE Metadata APIs without requiring any changes to the core HGE codebase. This allows connection to new (previously unsupported) databases by using an agent as middleware. In addition to this an agent can directly support new functionality without any other database upstream.

We hope that this feature enables developers to author new agents to support a wide variety of new databases and use-cases quickly and easily. The service based architecture should also allow for dynamic reconfiguration of what agents are connected, and what databases are supported as the circumstances dictate. This flexibility may be useful when a static set of supported drivers is not appropriate.

## Using the SDK

This SDK is intended to be used for the purposes or authoring new Data Connector Agents. The workflow that it supports out of the box is powered by [Docker Compose](https://docs.docker.com/compose/) in order to reduce the number of dependencies required, but each component may be run natively if desired.

The recommended workflow is as follows:

* Spin up the stack with `docker compose up`
* Check that the tests passed
* Make changes to the reference agent as required for your use-case (if you wish to use Typescript), or...
* Replace the reference agent with your own agent
* Rebuild the agent as required
* Rerun the tests with `docker compose run tests`
* Interact with the agent via GraphQL Engine exposed on http://localhost:8080
* Browse the OpenAPI schema on http://localhost:8300

The entire SDK is meant to serve as a template, so feel free to modify/remove any components as required.

### Making Changes to the Reference Agent

The Reference Agent source is available in `./reference` and the architecture is docuemented in `README_DATA_CONNECTORS.md`.
To adapt the existing agent to your needs, you should probably first look at the `/capabilities` and `/schema` endpoints
declared in `./reference/src/index.ts` - these then delegate implementation to `capabilities.ts` and `config.ts` respectively.
A guide to following this modification process is currently in development.

## SDK Components

* Documentation (`README.md` (this document), `README_DATA_CONNECTORS.md`) specifying:
    * How the components of the SDK have been assembled
    * How to spin up and run the tests
    * The architecture of the Data Connector feature
* OpenAPI types (`agent.openapi.json`)
* Reference Agent in Typescript (`reference-agent/`)
* Docker-Compose File Specifying:
    * Docker Image URI for Hasura GraphQL Engine (HGE)
    * Docker Image URI for Postgres DB (HGE Metadata Storage)
    * Docker Image URI for the Data Connector Agent Test Suite
    * Docker Image URI for the Data Connector Reference Agent
    * Docker Image URI for SwaggerUI hosting of Agent Schema Explorer
* A `.env` file that specifies the current build-version of the SDK resources
* Hasura GraphQL Engine Metadata for bootstrapping by adding the reference agent (`metadata/`)

### Component Breakdown

The SDK is comprised of several components and can be downloaded as a zip archive. Besides the individual components, the archive contains a `README.md` (this file), detailing how to set up and run each item, as well as information on how each item was produced.

The archive also contains a `docker-compose.yaml` file that spins up dockerized HGE, Postgres, Reference-Agent, Swagger-UI, and Agent-Tests and runs the tests. It then exposes the running HGE connected to the reference agent. This allows the user to see an example of how everything should be connected in practice and substitute in their own agent implementation for testing as it is being developed.

Note: If you are using a hybrid setup of native and docker services (especially when developing your own agent) you will need to ensure that your docker services are exposed correctly, and that you use the appropriate host URIs when pointing docker services to native services. For example: `host.docker.internal`

#### General Principles of Hasura Data Connector Agent Architecture and Development

While the reference agent should provide a fleshed out example of how an agent can be developed and what capabilities are possible, the following principles should also provide guidance how we recommend an agent be developed and structured:

* Capabilities & Self Describing - Your agent should describe itself via the `capabilities` feature.
* Stateless - Your agent should be (transparently) stateless, each request carries all the infomation required
* Defer logic to backend - Your agent should endevour to offload processing to its backend if possible
* Type-safe - Your agent should expect and return types as described in the OpenAPI schema
* Backwards compatible - Your agent should preserve backwards compatibility as it evolves
* Testing - Your agent should be tested with the provided test-suite

### OpenAPI Schema / Types

The OpenAPI Schema and Types are described in a formatted JSON file: `agent.openapi.json`

In addition to the OpenAPI schema file, a SwaggerUI interface is included in the `docker-compose.yaml` and is exposed on `http://localhost:8300`.


### Running the Tests

The architecture of the tests requires that a running agent is available. Once it is running the test-suite can be pointed to the agent to perform a suite of integration tests.

The tests will exercise various scenarios and agent capabilities according to the following logic:

* First, several mandatory scenarios will be run against the agent.
* If no additional options are given to the test-suite, then the test-suite will discover the agent’s capabilities (See Capabilities Section of README_DATA_CONNECTORS.md)
    * The agent’s advertised capabilities will then be enumerated over and the corresponding tests will be run to ensure that they are implemented correctly
    * If invalid capabilities are advertised then errors will be reported
* Capabilities can also be listed explicitly via CLI options to the test-suite
    * If this is done then the listed capabilities will be tested regardless of what the agent advertises
    * The difference in listed and advertised capabilities will be shown
    * This is useful to ensure that the agents advertised capabilities match your expectations of the agent
* If errors are found then they will be printed as the tests run.
* An error status code will be set if any errors are encountered.


#### Running the Tests with Docker

To run the tests against the reference agent, run `docker compose run tests` (this is also run on startup of `docker compose up`).


### Reference Agent

A copy of the reference agent is included in the folder `reference`. This is implemented in TypeScript and includes its own `README.md` file for configuring and running. While the reference agent is able to be run natively, for convenience, a docker container is available and the reference agent will be started when running `docker compose up`. If you wish to run the reference agent on its own you can run `docker compose run reference-agent`.

NOTE: You will need to expose the desired ports if you wish to connect to this agent from native system services.