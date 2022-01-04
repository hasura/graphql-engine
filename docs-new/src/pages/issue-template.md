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

2. Verify content correctness (compare against the current live version of docs) and preview in UI.
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
</details>

- [ ] [Core](https://github.com/hasura/graphql-engine/tree/master/docs-new/docs/graphql/core/) @KRRISH96 https://github.com/hasura/graphql-engine/pull/xyz


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