ARG SERVER_IMAGE_TAG

FROM hasura/graphql-engine:${SERVER_IMAGE_TAG}

RUN apt-get update && apt-get install -y netcat

# set an env var to let the cli know that
# update notification is disabled
ENV HASURA_GRAPHQL_SHOW_UPDATE_NOTIFICATION=false

COPY docker-entrypoint.sh /bin/
COPY cli-hasura-linux-amd64 /bin/hasura-cli

RUN chmod +x /bin/hasura-cli 

# set an env var to let the cli know that
# it is running in server environment
ENV HASURA_GRAPHQL_CLI_ENVIRONMENT=server-on-docker

ENTRYPOINT ["docker-entrypoint.sh"]

CMD ["graphql-engine", "serve"]