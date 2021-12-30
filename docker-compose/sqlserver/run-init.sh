#!/bin/bash

echo 'Attempting to run script (10 times max)'

sleep 5
for i in 1 2 3 4 5 6 7 8 9 10; do
  echo Will wait 5 seconds if this fails ...
  /opt/mssql-tools/bin/sqlcmd -S localhost,1433 -U SA -P "DockerComposePassword!" -i init.sql && break || sleep 5;
done

echo Finished attempts.
