#!/bin/bash
if [ -z "$TEST_HGE_URL" ];
then echo "ERROR: Please run the test command with the environment variable TEST_HGE_ENDPOINT";
else ../bin/run $TEST_HGE_URL --db=./db.js --overwrite && node verify.js;
fi
