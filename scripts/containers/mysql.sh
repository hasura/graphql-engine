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
MYSQL_CONTAINER_NAME="hasura-dev-mysql-$MYSQL_PORT"
# space deliberately omitted between -u and -p params https://hub.docker.com/_/mysql
MYSQL_DOCKER="docker exec -it $MYSQL_CONTAINER_NAME mysql -u$MYSQL_USER -p$MYSQL_PASSWORD"

######################
#     Functions      #
######################

function mysql_launch_container(){
  echo "Launching MySQL container: $MYSQL_CONTAINER_NAME"
  docker run --name $MYSQL_CONTAINER_NAME \
    -e MYSQL_ROOT_PASSWORD=$MYSQL_PASSWORD \
    -e MYSQL_DATABASE=hasura \
    -p 127.0.0.1:$MYSQL_PORT:3306 \
    -d mysql:8.0 -h "127.0.0.1"
}

function mysql_wait {
  echo -n "Waiting for mysql to come up"
  until ( $MYSQL_DOCKER -e 'SELECT 1' ) &>/dev/null; do
    echo -n '.' && sleep 0.2
  done
  echo " Ok"
}

function mysql_cleanup(){
  echo "Removing $MYSQL_CONTAINER_NAME and its volumes in 5 seconds!"
  echo "  PRESS CTRL-C TO ABORT removal of all containers, or ENTER to clean up right away"
  read -rt5 || true
  docker stop "$MYSQL_CONTAINER_NAME"
  docker rm -v "$MYSQL_CONTAINER_NAME"
}
