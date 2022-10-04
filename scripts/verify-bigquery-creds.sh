#!/usr/bin/env bash

set -e
set -u

# Suggested usage:
# https://github.com/hasura/graphql-engine/blob/master/server/lib/api-tests/README.md#required-setup-for-bigquery-tests
# https://cloud.google.com/iam/docs/creating-managing-service-accounts#iam-service-accounts-create-rest

if [[ -z "${HASURA_BIGQUERY_SERVICE_KEY:-}" ]]; then
  echo >&2 'You must set the HASURA_BIGQUERY_SERVICE_KEY environment variable.'
  echo >&2 'Please see the test README for details.'
  exit 2
fi

if [[ -z "${HASURA_BIGQUERY_PROJECT_ID:-}" ]]; then
  echo >&2 'You must set the HASURA_BIGQUERY_PROJECT_ID environment variable.'
  echo >&2 'Please see the test README for details.'
  exit 2
fi

project_id="$(jq -r -n --argjson service_account "$HASURA_BIGQUERY_SERVICE_KEY" '$service_account.project_id')"
service_account_email="$(jq -r -n --argjson service_account "$HASURA_BIGQUERY_SERVICE_KEY" '$service_account.client_email')"

if [[ "$HASURA_BIGQUERY_PROJECT_ID" != "$project_id" ]]; then
  echo >&2 'The HASURA_BIGQUERY_PROJECT_ID must be set to the same value as specified in the service key.'
  exit 2
fi

curl --fail \
  "https://content-bigquery.googleapis.com/bigquery/v2/projects/${project_id}/queries" \
  --json '{"query": "select 123"}' \
  -H "Authorization: Bearer $(gcloud auth print-access-token "$service_account_email" --project="$project_id")"
