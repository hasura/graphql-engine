#!/usr/bin/env bash

# helper functions used to run BigQuery tests
# https://github.com/hasura/graphql-engine/tree/master/server/tests-py#running-bigquery-tests


# === functions to test BigQuery locally and in CI

# checks that the required bigquery environment variables are available to run tests
verify_bigquery_pytest_env() {
    if [[ -z "${HASURA_BIGQUERY_SERVICE_KEY:-}" || -z "${HASURA_BIGQUERY_PROJECT_ID:-}"  ]]; then
        echo "HASURA_BIGQUERY_SERVICE_KEY and HASURA_BIGQUERY_PROJECT_ID environment variables are needed to run these tests."
        echo "See https://github.com/hasura/graphql-engine/tree/master/server/tests-py#running-bigquery-tests for more information."
        exit 1
    fi
}

# === functions to test BigQuery in CI with ephemeral Google Cloud projects

# checks that the optional bigquery environment variables are available to run tests against a temporary project
verify_temp_project_env() {
    if
      [[ -z "${HASURA_BIGQUERY_TEST_DIR:-}" \
      || -z "${HASURA_BIGQUERY_BILLING_ACCT:-}"  \
      || -z "${HASURA_BIGQUERY_API_KEY:-}"  \
      || -z "${HASURA_BIGQUERY_IAM_ACCOUNT:-}"  \
      ]]; then
        echo "the following environment variables are needed to create a temporary test project:"
        echo "HASURA_BIGQUERY_API_KEY: the API associated with the project"
        # https://cloud.google.com/docs/authentication/api-keys
        echo "HASURA_BIGQUERY_BILLING_ACCT: the billing account ID that should be linked to the project in order to run queries"
        # https://cloud.google.com/billing/docs/how-to/manage-billing-account
        echo "HASURA_BIGQUERY_IAM_ACCOUNT: application default credentials"
        # https://google.aip.dev/auth/4110
        echo "HASURA_BIGQUERY_TEST_DIR: the ID for the new project's parent directory"
        # https://cloud.google.com/iam/docs/resource-hierarchy-access-control
        exit 1
    fi
}

# generates a BigQuery project with a given or random ID
# https://cloud.google.com/resource-manager/docs/creating-managing-projects
generate_test_project() (
  set -e
  local tmp_id_file=${1}
  # project_id_part may be altered to meet gcloud ID requirements
  local project_id_part=${2:-$(uuidgen)}
  local project_id
  project_id=$(echo bq-"$project_id_part" | cut -c1-30 | tr '[:upper:]' '[:lower:]')

  echo ""
  echo "--- create a short-lived bigquery project id: $project_id"
  gcloud projects create "$project_id" --folder="$HASURA_BIGQUERY_TEST_DIR"

  # verify the project was created successfully
  gcloud projects describe "$project_id"

  # store the project_id in a temporary file, so it's accessible outside of this subshell
  echo "$project_id" > "$tmp_id_file"

  # link to a billing account so we can run queries
  # https://cloud.google.com/billing/docs
  gcloud beta billing projects link "$project_id"  --billing-account "$HASURA_BIGQUERY_BILLING_ACCT"
)

# create a dataset within a specified project
# https://cloud.google.com/bigquery/docs/datasets-intro
create_bigquery_dataset() (
  set -e
  local project_id=${1}
  local dataset_name=${2}
  echo ""
  echo "--- create a test dataset id: $project_id:$dataset_name"
  bq --location=US mk -d \
    --project_id "$project_id" \
    "$dataset_name"
)

# helper function to setup and verify a bigquery project
setup_temp_bigquery_project() (
  local tmp_id_file=${1}
  local dataset_name=${2}
  local project_id_part=${3:-$(uuidgen)}

  verify_temp_project_env
  generate_test_project "${tmp_id_file}" "$project_id_part"
  project_id="$(cat "$tmp_id_file")"
  create_bigquery_dataset "$project_id" "$dataset_name"
  ensure_bigquery_dataset "$project_id" "$dataset_name"

  # suggested usage for CI tests:
  # export HASURA_BIGQUERY_PROJECT_ID=$(cat $tmp_id_file)
)

delete_temp_bigquery_project() {
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
  local project_id=${1}
  local dataset_name=${2}

  echo "--- :database: ensure we can access the $dataset_name dataset in bigquery project $project_id"
  for _ in $(seq 1 60);
  do
    curl --fail --output /dev/null \
      "https://content-bigquery.googleapis.com/bigquery/v2/projects/$project_id/datasets/$dataset_name/tables?alt=json&key=$HASURA_BIGQUERY_API_KEY" \
      -H "Authorization: Bearer $(gcloud auth print-access-token "$HASURA_BIGQUERY_IAM_ACCOUNT" \
      --project="$project_id")" \
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
