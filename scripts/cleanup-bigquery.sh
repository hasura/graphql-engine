#!/usr/bin/env bash

# Clean up old BigQuery test datasets.

# If we end the bigquery API test suite abruptly, the created datasets won't be
# cleaned up. In which case, we end up with a bunch of `hasura_test_*` datasets
# that hang around forever. When we get too many of these, we can run this
# script, and it will delete them in batches. See the JavaScript file for more
# information.

gcloud auth login
gcloud config set project regency-polecat-beehive
node $(dirname "$0")/cleanup-bigquery.js
