# Cloud + Core Issue Template

Recently we made a call to migrate our docs tech stack from Sphinx (RST) to Docusaurus (MDX).

The crucial part of this process is actually transforming the `.rst` files to `.mdx` files.

Docs is pretty large (`76 directories, 402 files`). So, we would love your help! 💪

### How to contribute?
We created a [migration guide](https://hge-docs-migration.netlify.app/) to help ease the transition.

We used an auto transform tool [Pandoc](https://pandoc.org) to transform reStructiredText to GitHub flavoured markdown and added the resultant markdown content in respective `.txt` files.

You will be working in the `graphql-engine/docs-new/docs/graphql` directory. Here you can find the `.txt` files already added from auto-transform result. So, you don't have to create any new files.

Below is the steps to complete the migration cylcle for each doc file.

1. Update the extension and migrate content.

  1.1. Convert the transformed `.rst.txt` to `.mdx` extension for the file to be able to show up in sidebar and UI.

  1.2. Use the [migration guide](https://hge-docs-migration.netlify.app/) for syntactical and usage reference.

  1.3. Validate content and cleanup misc divs that are populated during auto transform and any invalid mdx data.

  1.4. The misc populated divs exist to ease adding metadata and any sphinx directives that can't be auto transformed to markdown. Please cross referene the actual RST file for content validation.

2. Verify content correctness (compare against the current live version of [Hasura docs](https://hasura.io/docs/)) and preview in UI.

3. Submit a PR! 🎉

### Available (includes WIP) Doc Files
We will periodically add new batches as we complete the existing ones here, so you don't have to search through super long list.

<details>
<summary>Hasura Cloud (`docs/graphql/cloud/`)</summary>

- [ ] [docs/graphql/cloud/api-reference.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/api-reference.rst)
- [ ] [docs/graphql/cloud/changelog.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/changelog.rst)
- [ ] [docs/graphql/cloud/dedicated-vpc.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/dedicated-vpc.rst)
- [ ] [docs/graphql/cloud/glossary.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/glossary.rst)
- [ ] [docs/graphql/cloud/index.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/index.rst)
- [ ] [docs/graphql/cloud/preview-apps.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/preview-apps.rst)
- [ ] [docs/graphql/cloud/query-tags.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/query-tags.rst)
- [ ] [docs/graphql/cloud/read-replicas.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/read-replicas.rst)
- [ ] [docs/graphql/cloud/regression-tests.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/regression-tests.rst)
- [ ] [docs/graphql/cloud/response-caching.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/response-caching.rst)
- [ ] [docs/graphql/cloud/tracing.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/tracing.rst)
- [ ] [docs/graphql/cloud/billing](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/billing)
    - [ ] [docs/graphql/cloud/billing/credits.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/billing/credits.rst)
    - [ ] [docs/graphql/cloud/billing/index.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/billing/index.rst)
    - [ ] [docs/graphql/cloud/billing/payment-methods.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/billing/payment-methods.rst)
    - [ ] [docs/graphql/cloud/billing/receipts.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/billing/receipts.rst)
- [ ] [docs/graphql/cloud/getting-started](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/getting-started)
    - [ ] [docs/graphql/cloud/getting-started/index.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/getting-started/index.rst)
    - [ ] [docs/graphql/cloud/getting-started/postgres-permissions.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/getting-started/postgres-permissions.rst)
    - [ ] [docs/graphql/cloud/getting-started/cloud-databases](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/getting-started/cloud-databases)
    - [ ] [docs/graphql/cloud/getting-started/cloud-databases/aiven.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/getting-started/cloud-databases/aiven.rst)
    - [ ] [docs/graphql/cloud/getting-started/cloud-databases/aws-aurora.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/getting-started/cloud-databases/aws-aurora.rst)
    - [ ] [docs/graphql/cloud/getting-started/cloud-databases/aws-postgres.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/getting-started/cloud-databases/aws-postgres.rst)
    - [ ] [docs/graphql/cloud/getting-started/cloud-databases/azure.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/getting-started/cloud-databases/azure.rst)
    - [ ] [docs/graphql/cloud/getting-started/cloud-databases/digital-ocean.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/getting-started/cloud-databases/digital-ocean.rst)
    - [ ] [docs/graphql/cloud/getting-started/cloud-databases/gcp.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/getting-started/cloud-databases/gcp.rst)
    - [ ] [docs/graphql/cloud/getting-started/cloud-databases/index.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/getting-started/cloud-databases/index.rst)
    - [ ] [docs/graphql/cloud/getting-started/cloud-databases/timescale-cloud.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/getting-started/cloud-databases/timescale-cloud.rst)
    - [ ] [docs/graphql/cloud/getting-started/cloud-databases/yugabyte.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/getting-started/cloud-databases/yugabyte.rst)
- [ ] [docs/graphql/cloud/hasurapro-cli](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/hasurapro-cli)
    - [ ] [docs/graphql/cloud/hasurapro-cli/index.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/hasurapro-cli/index.rst)
- [ ] [docs/graphql/cloud/metrics](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/metrics)
    - [ ] [docs/graphql/cloud/metrics/errors.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/metrics/errors.rst)
    - [ ] [docs/graphql/cloud/metrics/index.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/metrics/index.rst)
    - [ ] [docs/graphql/cloud/metrics/operations.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/metrics/operations.rst)
    - [ ] [docs/graphql/cloud/metrics/overview.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/metrics/overview.rst)
    - [ ] [docs/graphql/cloud/metrics/subscription-workers.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/metrics/subscription-workers.rst)
    - [ ] [docs/graphql/cloud/metrics/usage.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/metrics/usage.rst)
    - [ ] [docs/graphql/cloud/metrics/websockets.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/metrics/websockets.rst)
    - [ ] [docs/graphql/cloud/metrics/integrations](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/metrics/integrations)
    - [ ] [docs/graphql/cloud/metrics/integrations/azure-monitor.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/metrics/integrations/azure-monitor.rst)
    - [ ] [docs/graphql/cloud/metrics/integrations/datadog.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/metrics/integrations/datadog.rst)
    - [ ] [docs/graphql/cloud/metrics/integrations/index.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/metrics/integrations/index.rst)
    - [ ] [docs/graphql/cloud/metrics/integrations/newrelic.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/metrics/integrations/newrelic.rst)
- [ ] [docs/graphql/cloud/projects](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/projects)
    - [ ] [docs/graphql/cloud/projects/collaborators.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/projects/collaborators.rst)
    - [ ] [docs/graphql/cloud/projects/create.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/projects/create.rst)
    - [ ] [docs/graphql/cloud/projects/delete.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/projects/delete.rst)
    - [ ] [docs/graphql/cloud/projects/details.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/projects/details.rst)
    - [ ] [docs/graphql/cloud/projects/domains.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/projects/domains.rst)
    - [ ] [docs/graphql/cloud/projects/env-vars.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/projects/env-vars.rst)
    - [ ] [docs/graphql/cloud/projects/environments.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/projects/environments.rst)
    - [ ] [docs/graphql/cloud/projects/github-integration.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/projects/github-integration.rst)
    - [ ] [docs/graphql/cloud/projects/heroku-url-sync.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/projects/heroku-url-sync.rst)
    - [ ] [docs/graphql/cloud/projects/index.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/projects/index.rst)
    - [ ] [docs/graphql/cloud/projects/maintenance-mode.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/projects/maintenance-mode.rst)
    - [ ] [docs/graphql/cloud/projects/move-project-manual.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/projects/move-project-manual.rst)
    - [ ] [docs/graphql/cloud/projects/move-project-v2-manual.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/projects/move-project-v2-manual.rst)
    - [ ] [docs/graphql/cloud/projects/move-project-v2.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/projects/move-project-v2.rst)
    - [ ] [docs/graphql/cloud/projects/ownership.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/projects/ownership.rst)
    - [ ] [docs/graphql/cloud/projects/pricing.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/projects/pricing.rst)
    - [ ] [docs/graphql/cloud/projects/regions.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/projects/regions.rst)
    - [ ] [docs/graphql/cloud/projects/secure.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/projects/secure.rst)
- [ ] [docs/graphql/cloud/security](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/security)
    - [ ] [docs/graphql/cloud/security/allow-lists.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/security/allow-lists.rst)
    - [ ] [docs/graphql/cloud/security/api-limits.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/security/api-limits.rst)
    - [ ] [docs/graphql/cloud/security/disable-graphql-introspection.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/security/disable-graphql-introspection.rst)
    - [ ] [docs/graphql/cloud/security/index.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/security/index.rst)
    - [ ] [docs/graphql/cloud/security/rotating-admin-secrets.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/cloud/security/rotating-admin-secrets.rst)

</details>

<details>
<summary>Hasura Core (`docs/graphql/core/`)</summary>

- [ ] [docs/graphql/core/index.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/index.rst)
- [ ] [docs/graphql/core/actions](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/actions)
    - [ ] [docs/graphql/core/actions/action-examples.rst.wip](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/actions/action-examples.rst.wip)
    - [ ] [docs/graphql/core/actions/action-handlers.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/actions/action-handlers.rst)
    - [ ] [docs/graphql/core/actions/action-permissions.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/actions/action-permissions.rst)
    - [ ] [docs/graphql/core/actions/action-relationships.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/actions/action-relationships.rst)
    - [ ] [docs/graphql/core/actions/async-actions.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/actions/async-actions.rst)
    - [ ] [docs/graphql/core/actions/create.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/actions/create.rst)
    - [ ] [docs/graphql/core/actions/debugging.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/actions/debugging.rst)
    - [ ] [docs/graphql/core/actions/derive.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/actions/derive.rst)
    - [ ] [docs/graphql/core/actions/index.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/actions/index.rst)
    - [ ] [docs/graphql/core/actions/logs-clean-up.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/actions/logs-clean-up.rst)
    - [ ] [docs/graphql/core/actions/transforms.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/actions/transforms.rst)
    - [ ] [docs/graphql/core/actions/codegen](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/actions/codegen)
        - [ ] [docs/graphql/core/actions/codegen/index.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/actions/codegen/index.rst)
        - [ ] [docs/graphql/core/actions/codegen/python-flask.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/actions/codegen/python-flask.rst)
    - [ ] [docs/graphql/core/actions/types](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/actions/types)
        - [ ] [docs/graphql/core/actions/types/index.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/actions/types/index.rst)
- [ ] [docs/graphql/core/api-reference](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference)
    - [ ] [docs/graphql/core/api-reference/config.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/config.rst)
    - [ ] [docs/graphql/core/api-reference/explain.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/explain.rst)
    - [ ] [docs/graphql/core/api-reference/health.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/health.rst)
    - [ ] [docs/graphql/core/api-reference/index.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/index.rst)
    - [ ] [docs/graphql/core/api-reference/pgdump.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/pgdump.rst)
    - [ ] [docs/graphql/core/api-reference/restified.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/restified.rst)
    - [ ] [docs/graphql/core/api-reference/syntax-defs.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/syntax-defs.rst)
    - [ ] [docs/graphql/core/api-reference/version.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/version.rst)
    - [ ] [docs/graphql/core/api-reference/graphql-api](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/graphql-api)
        - [ ] [docs/graphql/core/api-reference/graphql-api/index.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/graphql-api/index.rst)
        - [ ] [docs/graphql/core/api-reference/graphql-api/mutation.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/graphql-api/mutation.rst)
        - [ ] [docs/graphql/core/api-reference/graphql-api/query.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/graphql-api/query.rst)
    - [ ] [docs/graphql/core/api-reference/metadata-api](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/metadata-api)
        - [ ] [docs/graphql/core/api-reference/metadata-api/actions.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/metadata-api/actions.rst)
        - [ ] [docs/graphql/core/api-reference/metadata-api/computed-field.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/metadata-api/computed-field.rst)
        - [ ] [docs/graphql/core/api-reference/metadata-api/custom-functions.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/metadata-api/custom-functions.rst)
        - [ ] [docs/graphql/core/api-reference/metadata-api/custom-types.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/metadata-api/custom-types.rst)
        - [ ] [docs/graphql/core/api-reference/metadata-api/dataerrors.csv](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/metadata-api/dataerrors.csv)
        - [ ] [docs/graphql/core/api-reference/metadata-api/event-triggers.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/metadata-api/event-triggers.rst)
        - [ ] [docs/graphql/core/api-reference/metadata-api/index.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/metadata-api/index.rst)
        - [ ] [docs/graphql/core/api-reference/metadata-api/inherited-roles.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/metadata-api/inherited-roles.rst)
        - [ ] [docs/graphql/core/api-reference/metadata-api/introspection.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/metadata-api/introspection.rst)
        - [ ] [docs/graphql/core/api-reference/metadata-api/manage-metadata.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/metadata-api/manage-metadata.rst)
        - [ ] [docs/graphql/core/api-reference/metadata-api/network.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/metadata-api/network.rst)
        - [ ] [docs/graphql/core/api-reference/metadata-api/permission.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/metadata-api/permission.rst)
        - [ ] [docs/graphql/core/api-reference/metadata-api/query-collections.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/metadata-api/query-collections.rst)
        - [ ] [docs/graphql/core/api-reference/metadata-api/relationship.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/metadata-api/relationship.rst)
        - [ ] [docs/graphql/core/api-reference/metadata-api/remote-relationships.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/metadata-api/remote-relationships.rst)
        - [ ] [docs/graphql/core/api-reference/metadata-api/remote-schema-permissions.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/metadata-api/remote-schema-permissions.rst)
        - [ ] [docs/graphql/core/api-reference/metadata-api/remote-schemas.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/metadata-api/remote-schemas.rst)
        - [ ] [docs/graphql/core/api-reference/metadata-api/restified-endpoints.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/metadata-api/restified-endpoints.rst)
        - [ ] [docs/graphql/core/api-reference/metadata-api/scheduled-triggers.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/metadata-api/scheduled-triggers.rst)
        - [ ] [docs/graphql/core/api-reference/metadata-api/source.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/metadata-api/source.rst)
        - [ ] [docs/graphql/core/api-reference/metadata-api/table-view.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/metadata-api/table-view.rst)
    - [ ] [docs/graphql/core/api-reference/relay-graphql-api](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/relay-graphql-api)
        - [ ] [docs/graphql/core/api-reference/relay-graphql-api/index.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/relay-graphql-api/index.rst)
        - [ ] [docs/graphql/core/api-reference/relay-graphql-api/mutation.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/relay-graphql-api/mutation.rst)
        - [ ] [docs/graphql/core/api-reference/relay-graphql-api/query.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/relay-graphql-api/query.rst)
    - [ ] [docs/graphql/core/api-reference/schema-api](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/schema-api)
        - [ ] [docs/graphql/core/api-reference/schema-api/index.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/schema-api/index.rst)
        - [ ] [docs/graphql/core/api-reference/schema-api/run-sql.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/schema-api/run-sql.rst)
    - [ ] [docs/graphql/core/api-reference/schema-metadata-api](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/schema-metadata-api)
        - [ ] [docs/graphql/core/api-reference/schema-metadata-api/actions.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/schema-metadata-api/actions.rst)
        - [ ] [docs/graphql/core/api-reference/schema-metadata-api/computed-field.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/schema-metadata-api/computed-field.rst)
        - [ ] [docs/graphql/core/api-reference/schema-metadata-api/custom-functions.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/schema-metadata-api/custom-functions.rst)
        - [ ] [docs/graphql/core/api-reference/schema-metadata-api/custom-types.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/schema-metadata-api/custom-types.rst)
        - [ ] [docs/graphql/core/api-reference/schema-metadata-api/dataerrors.csv](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/schema-metadata-api/dataerrors.csv)
        - [ ] [docs/graphql/core/api-reference/schema-metadata-api/event-triggers.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/schema-metadata-api/event-triggers.rst)
        - [ ] [docs/graphql/core/api-reference/schema-metadata-api/index.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/schema-metadata-api/index.rst)
        - [ ] [docs/graphql/core/api-reference/schema-metadata-api/manage-metadata.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/schema-metadata-api/manage-metadata.rst)
        - [ ] [docs/graphql/core/api-reference/schema-metadata-api/permission.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/schema-metadata-api/permission.rst)
        - [ ] [docs/graphql/core/api-reference/schema-metadata-api/query-collections.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/schema-metadata-api/query-collections.rst)
        - [ ] [docs/graphql/core/api-reference/schema-metadata-api/relationship.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/schema-metadata-api/relationship.rst)
        - [ ] [docs/graphql/core/api-reference/schema-metadata-api/remote-relationships.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/schema-metadata-api/remote-relationships.rst)
        - [ ] [docs/graphql/core/api-reference/schema-metadata-api/remote-schema-permissions.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/schema-metadata-api/remote-schema-permissions.rst)
        - [ ] [docs/graphql/core/api-reference/schema-metadata-api/remote-schemas.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/schema-metadata-api/remote-schemas.rst)
        - [ ] [docs/graphql/core/api-reference/schema-metadata-api/restified-endpoints.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/schema-metadata-api/restified-endpoints.rst)
        - [ ] [docs/graphql/core/api-reference/schema-metadata-api/run-sql.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/schema-metadata-api/run-sql.rst)
        - [ ] [docs/graphql/core/api-reference/schema-metadata-api/scheduled-triggers.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/schema-metadata-api/scheduled-triggers.rst)
        - [ ] [docs/graphql/core/api-reference/schema-metadata-api/table-view.rst](https://github.com/hasura/graphql-engine/tree/master/docs/graphql/core/api-reference/schema-metadata-api/table-view.rst)

</details>

### Completed Doc Files
The completed ones will be moved here periodically to have the available ones easy visibility.

<details>
<summary>Hasura Cloud (`docs/graphql/cloud/`)</summary>
</details>

<details>
<summary>Hasura Core (`docs/graphql/core/`)</summary>

- [x] [docs/graphql/core/actions/create](https://github.com/hasura/graphql-engine/tree/master/docs-new/docs/graphql/core/actions/create.mdx) @KRRISH96 https://github.com/hasura/graphql-engine/pull/xyz

</details>


### Reach out to us!

Feel free to reach if you have questions or need help getting started. You can leave comments here or you can tag me(@KRRISH96) in your PR if you need any help or you're not sure about something!

---

To avoid duplicate work please comment on which part you want to work on (as long as nobody else is working on it) so we can mark it as taken.