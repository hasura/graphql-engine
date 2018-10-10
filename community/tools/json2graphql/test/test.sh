#!/bin/bash
if [ -z "$TEST_HGE_URL" ] && [ -z "$TEST_X_HASURA_ACCESS_KEY" ]; then
  echo "ERROR: Please run the test command with the environment variable TEST_HGE_URL"
else
  ../bin/run $TEST_HGE_URL --access-key=$TEST_X_HASURA_ACCESS_KEY --db=./db.js --overwrite && node verify.js
fi
