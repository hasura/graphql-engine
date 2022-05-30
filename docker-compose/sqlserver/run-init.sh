#!/bin/bash

echo 'Attempting to run script (10 times max)'
PORT="${1:-1433}"
SLEEP="3"

sleep "${SLEEP}"
for i in 1 2 3 4 5 6 7 8 9 10; do
  echo Attempt "#$i" - will wait "${SLEEP}" seconds if this fails ...
  sqlcmd -S 127.0.0.1,"${PORT}" -U SA -P "DockerComposePassword!" -i init.sql && break || sleep "${SLEEP}";
done

echo Finished attempts.
