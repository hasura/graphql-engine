FROM hasura/graphql-engine:v1.3.0-beta.3

# set an env var to let the cli know that
# it is running in server environment
ENV HASURA_GRAPHQL_CLI_ENVIRONMENT=server-on-docker
ENV HASURA_GRAPHQL_SHOW_UPDATE_NOTIFICATION=false

COPY docker-entrypoint.sh /bin/
COPY cli-hasura-linux-amd64 /bin/hasura-cli
RUN chmod +x /bin/hasura-cli

ENTRYPOINT ["docker-entrypoint.sh"]

CMD ["graphql-engine", "serve"]
