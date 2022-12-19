# Contributing

Hasura GraphQL Engine docs are built using [Docusaurus 2](https://docusaurus.io/) and are written in MDX. The most
up-to-date information for the docs site and how to contribute can be found on our wiki. You can access this on the
[live docs site](https://hasura.io/docs/wiki/), or [directly within this repository](wiki/index.mdx).

## Docs issues in the repo

Issues in the repo for documentation are labelled as `c/docs` (see
[list](https://github.com/hasura/graphql-engine/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen++label%3Ac%2Fdocs)). Issues
also labelled as `good first issue` are aimed at those making their first contribution to the repo (see
[list](https://github.com/hasura/graphql-engine/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen++label%3Ac%2Fdocs+label%3A%22good+first+issue%22)).
Issues also marked as `help wanted` (see
[list](https://github.com/hasura/graphql-engine/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen++label%3Ac%2Fdocs+label%3A%22help+wanted%22))
are those that require community contributions.

Please note that some of these issues, labelled with both `c/docs` and `c/console`/`c/server`, are part of a change/task
that requires modifications in some component of GraphQL Engine and the docs.

Feel free to open pull requests to address these issues or to add/fix docs features/content, even if a corresponding
issue doesn't exist. If you are unsure about whether to go ahead and work on something like the latter, please get in
touch with the maintainers in the `GraphQL Engine`->`contrib` channel in the community
[Discord](https://discord.gg/vBPpJkS).

## Notes

- Docs are currently deployed manually. Changes will not reflect immediately after a PR gets merged.
- The search is powered by [Algolia](https://www.algolia.com/) and is updated everyday. Your local changes will not be
  reflected in search results.
