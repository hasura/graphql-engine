- [Introduction to BigQuery](#introduction-to-bigquery)
- [Getting started](#getting-started)
- [Getting started with BigQuery on Hasura](#getting-started-with-bigquery-on-hasura)

### Introduction to BigQuery
BigQuery is a fully-managed, serverless data warehouse that enables high-performance, scalable analysis over petabytes of data. It is a Platform as a Service that supports querying using [SQL](#sql-dialect).

- [Google Cloud product page](https://cloud.google.com/bigquery/)
- [BigQuery vs Postgres comparison](https://weld.app/blog/postgresql-vs-bigquery)

### Getting started
BigQuery setup and usage is atypical compared to other relational database backends Hasura currently supports, such as Postgres & SQL Server. You may find these guides useful to familiarise yourself with BigQuery before contributing a PR.

- [Google Cloud Quickstart Guide](https://cloud.google.com/resource-manager/docs/quickstart-organizations)

Google Cloud projects form the basis for creating, enabling, and using all Google Cloud services, including BigQuery datasets.
- [Creating and managing projects](https://cloud.google.com/resource-manager/docs/creating-managing-projects)
- [Working with the resource hierarchy](https://cloud.google.com/resource-manager/docs/cloud-platform-resource-hierarchy)
- [Identity Access Management policy](https://cloud.google.com/resource-manager/docs/access-control-proj)
- [`gcloud`](https://cloud.google.com/sdk/gcloud), a set of command line tools to create and manage Google Cloud resources
- [Google Cloud Console: Resource Manager](https://console.cloud.google.com/cloud-resource-manager)

Once you have a project set up, you can work with datasets via the:
- [Google Cloud Console](https://cloud.google.com/bigquery/docs/quickstarts/quickstart-cloud-console)
- [REST API](https://cloud.google.com/bigquery/docs/reference/rest)
- [`bq`](https://cloud.google.com/bigquery/docs/quickstarts/load-data-bq), a command line tool to run queries and load data into BigQuery

### Getting started with BigQuery on Hasura
See the [hasura.io: BigQuery getting started guide](https://hasura.io/docs/latest/graphql/core/databases/bigquery/getting-started.html) for general information about connecting to a BigQuery data source.

Integration tests are run against short-lived projects. The following prerequisites are expected:
- A Google Cloud Console service account
- `HASURA_BIGQUERY_PROJECT_ID` environment variable
- `HASURA_BIGQUERY_SERVICE_KEY` environment variable

See [these docs](https://github.com/hasura/graphql-engine/tree/master/server/tests-py#running-bigquery-tests) for more guidance on testing against a BigQuery data source.
