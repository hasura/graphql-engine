#!/usr/bin/env bash
### This file is not meant to be run directly, but to be sourced from
### the dev script. It defines all the functions required to run an
### MySQL docker container.


######################
#    Configuration   #
######################

if [ "$MODE" = "test" ]; then
  MYSQL_PORT=33306
else
  MYSQL_PORT=23306
fi

MYSQL_USER=root
MYSQL_PASSWORD=hasuraMySQL1
MYSQL_VOLUME_NAME='hasura-dev-mysql'
MYSQL_CONTAINER_NAME="hasura-dev-mysql-$MYSQL_PORT"
# space deliberately omitted between -u and -p params https://hub.docker.com/_/mysql
MYSQL_DOCKER="docker exec -it $MYSQL_CONTAINER_NAME mysql -u$MYSQL_USER -p$MYSQL_PASSWORD"
MYSQL_CONTAINER_IMAGE=mysql:8.0
MYSQL_PLATFORM=linux/amd64 # for M1 arch we use the rosetta emulation

######################
#     Functions      #
######################

function mysql_launch_container {
  echo "Launching MySQL container: $MYSQL_CONTAINER_NAME"
  docker volume create "$MYSQL_VOLUME_NAME"
  docker run \
    --name "$MYSQL_CONTAINER_NAME" \
    --platform="$MYSQL_PLATFORM" \
    --publish="127.0.0.1:${MYSQL_PORT}:3306" \
    --volume="${MYSQL_VOLUME_NAME}:/var/lib/mysql" \
    --env=MYSQL_ROOT_PASSWORD="$MYSQL_PASSWORD" \
    --env=MYSQL_DATABASE='hasura' \
    --detach \
    "$MYSQL_CONTAINER_IMAGE" \
    -h "localhost"
}

function mysql_wait {
  echo -n "Waiting for mysql to come up"
  until ( $MYSQL_DOCKER -e 'SELECT 1' ) &>/dev/null; do
    echo -n '.' && sleep 0.2
  done
  echo " Ok"
}

function mysql_cleanup {
  echo "Removing $MYSQL_CONTAINER_NAME and its volumes in 5 seconds!"
  echo "  PRESS CTRL-C TO ABORT removal of all containers, or ENTER to clean up right away"
  read -rt5 || true
  docker stop "$MYSQL_CONTAINER_NAME"
  docker rm -v "$MYSQL_CONTAINER_NAME"
  docker volume rm "$MYSQL_VOLUME_NAME"
}
