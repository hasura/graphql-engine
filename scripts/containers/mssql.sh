#!/usr/bin/env bash
### This file is not meant to be run directly, but to be sourced from
### the dev script. It defines all the functions required to run an
### MSSQL docker container.


######################
#    Configuration   #
######################

if [ "$MODE" = "test" ]; then
  MSSQL_PORT=31433
else
  MSSQL_PORT=21433
fi

MSSQL_HOST=127.0.0.1
MSSQL_PASSWORD=hasuraMSSQL1
MSSQL_VOLUME_NAME='hasura-dev-mssql'
MSSQL_CONTAINER_NAME="hasura-dev-mssql-$MSSQL_PORT"
# shellcheck disable=SC2034  # this variable is used in scripts sourcing this one
MSSQL_CONN_STR="DRIVER={ODBC Driver 17 for SQL Server};SERVER=$MSSQL_HOST,$MSSQL_PORT;Uid=sa;Pwd=$MSSQL_PASSWORD;"
MSSQL_DOCKER="docker exec -it $MSSQL_CONTAINER_NAME sqlcmd -S localhost -U sa -P $MSSQL_PASSWORD"

if [[ "$(uname -m)" == 'arm64' ]]; then
  MSSQL_PLATFORM=linux/arm64
  MSSQL_CONTAINER_IMAGE=mcr.microsoft.com/azure-sql-edge
else
  MSSQL_PLATFORM=linux/amd64
  MSSQL_CONTAINER_IMAGE=hasura/mssql-server-2019-cu10-ubuntu-20.04:latest
fi

######################
#     Functions      #
######################

function mssql_launch_container {
  echo_pretty "Launching MSSQL container: $MSSQL_CONTAINER_NAME"
  docker volume create "$MSSQL_VOLUME_NAME"
  docker run \
    --name $MSSQL_CONTAINER_NAME \
    --platform "$MSSQL_PLATFORM" \
    --publish="${MSSQL_HOST}:${MSSQL_PORT}:1433" \
    --volume="${MSSQL_VOLUME_NAME}:/var/opt/mssql" \
    --env=ACCEPT_EULA=1 \
    --env="MSSQL_SA_PASSWORD=$MSSQL_PASSWORD" \
    --detach \
    "$MSSQL_CONTAINER_IMAGE"
}

function mssql_wait {
  echo -n "Waiting for mssql to come up"
  until ( $MSSQL_DOCKER -Q 'SELECT 1' ) &>/dev/null; do
    echo -n '.' && sleep 0.2
  done
  echo " Ok"
}

function mssql_cleanup {
  echo_pretty "Removing $MSSQL_CONTAINER_NAME and its volumes in 5 seconds!"
  echo_pretty "  PRESS CTRL-C TO ABORT removal of all containers, or ENTER to clean up right away"
  read -rt5 || true
  docker stop "$MSSQL_CONTAINER_NAME"
  docker rm -v "$MSSQL_CONTAINER_NAME"
  docker volume rm "$MSSQL_VOLUME_NAME"
}
