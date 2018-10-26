#!/bin/bash
if [ -z "$TEST_HGE_URL" ] && [ -z "$TEST_X_HASURA_ACCESS_KEY" ]; then
  echo "ERROR: Please run the test command with the environment variable TEST_HGE_URL"
else
  F2G_LOG=0 ../bin/run $TEST_HGE_URL --access-key=$TEST_X_HASURA_ACCESS_KEY --db=./data-sets/chinook.json --overwrite --normalize && node verifyChinook.js
  F2G_LOG=0 ../bin/run $TEST_HGE_URL --access-key=$TEST_X_HASURA_ACCESS_KEY --db=./data-sets/blog.json --overwrite --normalize && node verifyBlog.js
  F2G_LOG=0 ../bin/run $TEST_HGE_URL --access-key=$TEST_X_HASURA_ACCESS_KEY --db=./data-sets/chinook_nested.json --overwrite --normalize && node verifyChinookNested.js
  F2G_LOG=0 ../bin/run $TEST_HGE_URL --access-key=$TEST_X_HASURA_ACCESS_KEY --db=./data-sets/readme-example-1.json --overwrite --normalize && node verifyRE1.js
fi
