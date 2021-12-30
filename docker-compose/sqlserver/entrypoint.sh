#!/bin/bash

# Run init-script with long timeout - and make it run in the background
bash /run-init.sh &

# Start SQL server, quitely
/opt/mssql/bin/sqlservr 2>&1 > /dev/null
