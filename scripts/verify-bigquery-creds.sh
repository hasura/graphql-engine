#!/usr/bin/env bash
# Suggested usage:
# https://github.com/hasura/graphql-engine/blob/master/server/py-tests/README.md#running-bigquery-tests
# https://cloud.google.com/iam/docs/creating-managing-service-accounts#iam-service-accounts-create-rest

project_id=${1}
api_key=${2}
service_account_email=${3} # eg. "<<SERVICE_ACCOUNT_NAME>>@<<PROJECT_NAME>>.iam.gserviceaccount.com"

curl "https://content-bigquery.googleapis.com/bigquery/v2/projects/$project_id/queries?alt=json&key=$api_key" \
  --data-binary '{"query":"select 123"}' \
  -H "Authorization: Bearer $(gcloud auth print-access-token "$service_account_email" --project="$project_id")"
