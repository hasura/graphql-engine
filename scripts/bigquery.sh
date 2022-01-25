#!/usr/bin/env bash

# helper functions used to run BigQuery tests
# https://github.com/hasura/graphql-engine/tree/master/server/tests-py#running-bigquery-tests


# === functions to test BigQuery locally and in CI

# checks that the required bigquery environment variables are available to run tests
function verify_bigquery_pytest_env() {
    if [[ -z "${HASURA_BIGQUERY_SERVICE_ACCOUNT_FILE:-}" || -z "${HASURA_BIGQUERY_PROJECT_ID:-}"  ]]; then
        echo "HASURA_BIGQUERY_SERVICE_ACCOUNT_FILE and HASURA_BIGQUERY_PROJECT_ID environment variables are needed to run these tests."
        echo "See https://github.com/hasura/graphql-engine/tree/master/server/tests-py#running-bigquery-tests for more information."
        exit 1
    fi
}

# === functions to test BigQuery in CI with ephemeral Google Cloud projects

# checks that the optional bigquery environment variables are available to run tests against a temporary project
# HASURA_BIGQUERY_TEST_DIR is the ID for the new project's parent directory
#   https://cloud.google.com/iam/docs/resource-hierarchy-access-control
# HASURA_BIGQUERY_BILLING_ACCT is the billing account ID that should be linked to the project in order to run queries
#   https://cloud.google.com/billing/docs/how-to/manage-billing-account
function verify_temp_project_env() {
    if [[ -z "${HASURA_BIGQUERY_TEST_DIR:-}" || -z "${HASURA_BIGQUERY_BILLING_ACCT:-}"  ]]; then
        echo "HASURA_BIGQUERY_TEST_DIR and HASURA_BIGQUERY_BILLING_ACCT environment variables are needed to create a temporary test project."
        exit 1
    fi
}

function generate_test_project() {
  # NOTE: project_id_part may be shortened & altered to meet gcloud project ID requirements:
  # https://cloud.google.com/resource-manager/docs/creating-managing-projects
  local project_id_part=${1:-$(uuidgen)}
  HASURA_BIGQUERY_PROJECT_ID=$(echo bq-"$project_id_part" | cut -c1-30 | tr '[:upper:]' '[:lower:]')
  echo ""
  echo "--- create a short-lived bigquery project id: $HASURA_BIGQUERY_PROJECT_ID"
  gcloud projects create "$HASURA_BIGQUERY_PROJECT_ID" --folder="$HASURA_BIGQUERY_TEST_DIR"
  # projects require linking to a billing account to run any queries
  # https://cloud.google.com/billing/docs
  gcloud beta billing projects link "$HASURA_BIGQUERY_PROJECT_ID"  --billing-account "$HASURA_BIGQUERY_BILLING_ACCT"
  export HASURA_BIGQUERY_PROJECT_ID
}

function create_hasura_test_dataset() {
  local name='hasura_test' # all bigquery tests expect a dataset to exist with this name
  echo ""
  echo "--- create a test dataset id: $HASURA_BIGQUERY_PROJECT_ID:$name"
  bq --location=US mk -d \
    --project_id "$HASURA_BIGQUERY_PROJECT_ID" \
    "$name"
  echo "ok"
}

function create_temp_bigquery_project() {
  local project_id_part=${1:-$(uuidgen)}
  verify_temp_project_env
  generate_test_project "$project_id_part"
  create_hasura_test_dataset
}

function delete_temp_bigquery_project() {
  local project_id=${1}
  echo ""
  echo "--- delete bigquery project id: $project_id"
  gcloud projects delete "$project_id" --quiet
}
