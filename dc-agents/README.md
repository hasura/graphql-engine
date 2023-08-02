# Data Connector SDK

This SDK serves as a pack of documentation and resources for understanding, building, and testing Data Connector agent implementations to ensure that they are complete, correct, and idiomatic and can be developed rapidly and with confidence. The SDK is intended to evolve with time, gaining features and changing format to reflect the best practices for agent-development recommended by Hasura. As such, the SDK will be versioned and correlate with HGE versions via the included `.env` file.

In addition to the SDK archive, some information may be mirrored in the [docs.hasura](https://hasura.io/docs/latest/graphql/core/index/) documentation.

## What is a Data Connector (Agent)?

A Hasura GraphQL Engine Data Connector Agent is a service that abstracts a datasource behind a REST API and well-defined wire format. Further information can be found in `README_DATA_CONNECTORS.md` which describes the design and architecture of the feature. An agent facilitates the extension of database support for Hasura GraphQL Engine by allowing configuration of Agents at runtime through the HGE Metadata APIs without requiring any changes to the core HGE codebase. This allows connection to new (previously unsupported) databases by using an agent as middleware. In addition to this an agent can directly support new functionality without any other database upstream.

We hope that this feature enables developers to author new agents to support a wide variety of new databases and use-cases quickly and easily. The service based architecture should also allow for dynamic reconfiguration of what agents are connected, and what databases are supported as the circumstances dictate. This flexibility may be useful when a static set of supported drivers is not appropriate.

## Hasura Hub

The Hasura hub is intended to be a list of current data connector agents that are supported and work with graphql-engine. You can find that list in the [HUB.md](./HUB.md) doc.

## Using the SDK

This SDK is intended to be used for the purposes or authoring new Data Connector Agents. The workflow that it supports out of the box is powered by [Docker Compose](https://docs.docker.com/compose/) in order to reduce the number of dependencies required, but each component may be run natively if desired.

The recommended workflow is as follows:

- Spin up the stack with `docker compose up`
- Check that the tests passed
- Make changes to the reference agent as required for your use-case (if you wish to use Typescript), or...
- Replace the reference agent with your own agent
- Rebuild the agent as required
- Rerun the tests with `docker compose run tests`
- Interact with the agent via GraphQL Engine exposed on http://localhost:8080
- Browse the OpenAPI schema on http://localhost:8300

The entire SDK is meant to serve as a template, so feel free to modify/remove any components as required.

More on SDK documentation [here](./sdk/README.md)

## Documentation

You can read the documentation on data connector agents and the api in the [DOCUMENTATION.md](./DOCUMENTATION.md) file.
