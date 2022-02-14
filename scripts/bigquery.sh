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
  local tmp_id_file=${2}
  HASURA_BIGQUERY_PROJECT_ID=$(echo bq-"$project_id_part" | cut -c1-30 | tr '[:upper:]' '[:lower:]')
  echo ""
  echo "--- create a short-lived bigquery project id: $HASURA_BIGQUERY_PROJECT_ID"
  gcloud projects create "$HASURA_BIGQUERY_PROJECT_ID" --folder="$HASURA_BIGQUERY_TEST_DIR"

  # projects require linking to a billing account to run any queries
  # https://cloud.google.com/billing/docs
  gcloud beta billing projects link "$HASURA_BIGQUERY_PROJECT_ID"  --billing-account "$HASURA_BIGQUERY_BILLING_ACCT"
  # checking project existence
  gcloud projects describe "$HASURA_BIGQUERY_PROJECT_ID" --quiet
  export HASURA_BIGQUERY_PROJECT_ID
  echo "$HASURA_BIGQUERY_PROJECT_ID" > "$tmp_id_file"
}

function create_bigquery_dataset() {
  local dataset_name=${1}
  echo ""
  echo "--- create a test dataset id: $HASURA_BIGQUERY_PROJECT_ID:$dataset_name"
  bq --location=US mk -d \
    --project_id "$HASURA_BIGQUERY_PROJECT_ID" \
    "$dataset_name"
  echo "ok"
}

function create_temp_bigquery_project() {
  local project_id_part=${1:-$(uuidgen)}
  local dataset_name=${2}
  local tmp_id_file=${3}
  verify_temp_project_env
  generate_test_project "$project_id_part" "${tmp_id_file}"
  create_bigquery_dataset "$dataset_name"
}

function delete_temp_bigquery_project() {
  local project_id=${1}
  echo ""
  echo "--- delete bigquery project id: $project_id"
  gcloud projects delete "$project_id" --quiet
}


authenticate_bigquery() {
  local tests_dir=${1}
  echo "--- :unlock: authenticate bigquery service account"
  export HASURA_BIGQUERY_SERVICE_ACCOUNT_FILE="gcloud-service-key.json"

  pushd "$tests_dir" || { echo "Couldn't pushd to $tests_dir"; exit 1; }
  echo "${HASURA_BIGQUERY_SERVICE_KEY}" > "$HASURA_BIGQUERY_SERVICE_ACCOUNT_FILE"
  gcloud auth activate-service-account --key-file="$HASURA_BIGQUERY_SERVICE_ACCOUNT_FILE" || { echo "Couldn't authenticate on GCloud"; exit 1; }
  popd || { echo "Couldn't popd"; exit 1; }
}

ensure_bigquery_dataset() {
  local dataset_name=${1}
  echo "--- :database: ensure the bigquery data source is accessible, i.e. we can access the $dataset_name dataset in bigquery project"
  for _ in $(seq 1 60);
  do
    curl --fail --output /dev/null \
      "https://content-bigquery.googleapis.com/bigquery/v2/projects/$HASURA_BIGQUERY_PROJECT_ID/datasets/$dataset_name/tables?alt=json&key=$HASURA_BIGQUERY_API_KEY" \
      -H "Authorization: Bearer $(gcloud auth print-access-token "$HASURA_BIGQUERY_IAM_ACCOUNT" \
      --project="$HASURA_BIGQUERY_PROJECT_ID")" \
      && echo "Success" && return 0
    echo -n .
    sleep 1
  done
  echo "Failed waiting for bigquery dataset"
  exit 1
}

remove_temp_project_with_id_in_file() {
  local tmp_id_file="$1"
  if [ -f "$tmp_id_file" ]; then
    # necessary as $HASURA_BIGQUERY_PROJECT_ID is changed in the subshell
    delete_temp_bigquery_project "$(cat "$tmp_id_file")"
    rm "$tmp_id_file"
  fi
}
