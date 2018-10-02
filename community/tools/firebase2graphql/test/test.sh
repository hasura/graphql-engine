#!/bin/bash
if [ -z "$TEST_HGE_URL" ] && [ -z "$TEST_X_HASURA_ACCESS_KEY" ]; then
  echo "ERROR: Please run the test command with the environment variable TEST_HGE_URL"
else
  ../bin/run $TEST_HGE_URL --access-key=$TEST_X_HASURA_ACCESS_KEY --db=./data-sets/chinook.json --overwrite && node verifyChinook.js
  ../bin/run $TEST_HGE_URL --access-key=$TEST_X_HASURA_ACCESS_KEY --db=./data-sets/blog.json --overwrite && node verifyBlog.js
fi
