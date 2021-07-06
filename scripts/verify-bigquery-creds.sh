#!/usr/bin/env bash
# Suggested usage:
# https://github.com/hasura/graphql-engine/blob/master/server/CONTRIBUTING.md#running-the-python-test-suite-on-bigquery

export YOURPROJECT="<<PROJECT_ID>>"
export YOURACCOUNT="<<SERVICE_ACCOUNT_NAME>>@<<PROJECT_NAME>>.iam.gserviceaccount.com"
export YOURAPIKEY="<<API_KEY>>"

curl "https://content-bigquery.googleapis.com/bigquery/v2/projects/$YOURPROJECT/queries?alt=json&key=$YOURAPIKEY" --data-binary '{"query":"select 123"}' -H "Authorization: Bearer $(gcloud auth print-access-token $YOURACCOUNT --project=$YOURPROJECT)" -H 'content-type: application/json'
