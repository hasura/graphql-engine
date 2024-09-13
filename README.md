![Hasura logo](./assets/hasura_logo_primary_darkbg.png#gh-dark-mode-only)
![Hasura logo](./assets/hasura_logo_primary_lightbg.png#gh-light-mode-only)

# Hasura GraphQL Engine

The Hasura engine is an open source project which supercharges the building of modern applications by providing access
to data via a single, composable, secure API endpoint.

<a href="https://hasura.io/"><img src="https://img.shields.io/badge/🏠_Visit-Hasura_Homepage-blue.svg?style=flat"></a>
<a href="https://hasura.io/community/"><img src="https://img.shields.io/badge/😊_Join-Community-blue.svg?style=flat"></a>

## Hasura V3

[![Docs](https://img.shields.io/badge/docs-v3-yellow.svg?style=flat)](https://hasura.io/docs/3.0/getting-started/quickstart/)

The future of data delivery is GA: Supporting PostgreSQL (and its flavors), MongoDB, ClickHouse, and MS SQL Server. Also supports writing custom business logic using the Typescript, Python, and Go Connector SDKs. Here is the recommended [Getting Started](https://hasura.io/docs/3.0/getting-started/quickstart/) guide on DDN.

The Hasura v3 engine code, which powers Hasura DDN, is in the `v3` folder of this repo. You can find more detailed
information about in this [v3 README](/v3/README.md).

The Hasura DDN architecture includes Data Connectors to connect to data sources. All Hasura connectors are also
available completely open source. Check out the [Connector Hub](https://hasura.io/connectors/) which lists all
available connectors.

## Hasura V2

[![Latest release](https://img.shields.io/github/v/release/hasura/graphql-engine)](https://github.com/hasura/graphql-engine/releases/latest)
[![Docs](https://img.shields.io/badge/docs-v2.x-yellow.svg?style=flat)](https://hasura.io/docs)

Hasura V2 is the current stable version of the Hasura GraphQL Engine. Please find more
detailed information about the V2 Hasura Graphql Engine in the `v2` folder and this [README](V2-README.md).

## Cloning repository

This repository is a large and active mono-repo containing many parts of the Hasura ecosystem and a long git
history, that can make the first time cloning of the repository slow and consume a lot of disk space. We recommend
following if you are facing cloning issues.

### Shallow clone

This will only clone the latest commit and ignore all historical commits.

```
git clone https://github.com/hasura/graphql-engine.git --depth 1
```

### Git checkout with only Hasura V3 engine code

```
git clone --no-checkout https://github.com/hasura/graphql-engine.git --depth 1
cd graphql-engine
git sparse-checkout init --cone
git sparse-checkout set v3
git checkout @
```

This checkouts the top level files and only the `v3` folder which contains the Hasura V3 Engine code.

## Support & Troubleshooting

To troubleshoot most issues, check out our documentation and community resources. If you have encountered a bug or need
to get in touch with us, you can contact us using one of the following channels:

- Hasura DDN documentation: [DDN docs](https://hasura.io/docs/3.0/)
- Hasura V2 documentation: [V2 docs](https://hasura.io/docs/)
- Support & feedback: [Discord](https://discord.gg/hasura)
- Issue & bug tracking: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
- Follow product updates: [@HasuraHQ](https://twitter.com/hasurahq)
- Talk to us on our [website chat](https://hasura.io)

## Code of Conduct

We are committed to fostering an open and welcoming environment in the community. Please see the
[Code of Conduct](code-of-conduct.md).

## Security

If you want to report a security issue, please [read this](SECURITY.md).

## Stay up to date

Join our communities to stay up to date on announcements, events, product updates, and technical blogs.
[https://hasura.io/community/](https://hasura.io/community/)

## Contributing

Check out our [contributing guide](CONTRIBUTING.md) for more details.

## Brand assets

Hasura brand assets (logos, the Hasura mascot, powered by badges etc.) can be found in the
[v2/assets/brand](assets/brand) folder. Feel free to use them in your application/website etc. We'd be thrilled if you
add the "Powered by Hasura" badge to your applications built using Hasura. ❤️

## Licenses

### V3

All the [Data Connectors](https://github.com/hasura/ndc-hub) are available under
the [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0).

The core [V3 GraphQL Engine](v3/) is intended to be licensed under the [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

### V2

The V2 core GraphQL Engine is available under the [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

All **other contents** in the v2 folder (except those in [`server`](v2/server), [`cli`](v2/cli) and
[`console`](v2/console) directories) are available under the [MIT License](LICENSE-community).
This includes everything in the [`docs`](v2/docs) and [`community`](v2/community)
directories.
