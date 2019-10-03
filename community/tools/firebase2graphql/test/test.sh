#!/bin/bash
if [ -z "$TEST_HGE_URL" ] && [ -z "$TEST_X_HASURA_ADMIN_SECRET" ]; then
  echo "ERROR: Please run the test command with the environment variable TEST_HGE_URL"
else
  F2G_LOG=0 ../bin/run $TEST_HGE_URL --admin-secret=$TEST_X_HASURA_ADMIN_SECRET --db=./data-sets/chinook.json --overwrite --normalize && node verifyChinook.js
  F2G_LOG=0 ../bin/run $TEST_HGE_URL --admin-secret=$TEST_X_HASURA_ADMIN_SECRET --db=./data-sets/blog.json --overwrite --normalize && node verifyBlog.js
  F2G_LOG=0 ../bin/run $TEST_HGE_URL --admin-secret=$TEST_X_HASURA_ADMIN_SECRET --db=./data-sets/chinook_nested.json --overwrite --normalize && node verifyChinookNested.js
  F2G_LOG=0 ../bin/run $TEST_HGE_URL --admin-secret=$TEST_X_HASURA_ADMIN_SECRET --db=./data-sets/readme-example-1.json --overwrite --normalize && node verifyRE1.js
fi
