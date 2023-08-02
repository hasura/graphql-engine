#!/bin/bash

SETUP_SCRIPT=/opt/mssql_scripts/mssql-setup.sql
DONE_SCRIPT=/opt/mssql_scripts/REMOVE_ME_TO_RERUN_SETUP.txt

# Wait to be sure that SQL Server came up
sleep 15s

# Run the setup script to create the DB and the schema in the DB
echo Checking if sql script has been setup...

if test -f "$DONE_SCRIPT"; then
   echo "Setup has already been performed. Delete file 'REMOVE_ME_TO_RERUN_SETUP.txt' to rerun setup."
else
   echo "$SETUP_SCRIPT has not yet run. Running setup script"
   /opt/mssql-tools/bin/sqlcmd -I -S mssql -U sa -P Password! -i "$SETUP_SCRIPT"

   # after script runs, create a dummy file to keep track of state
   echo >> $DONE_SCRIPT
fi

echo SQL Server completed initialization.