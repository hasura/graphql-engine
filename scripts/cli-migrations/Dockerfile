FROM hasura/graphql-engine:v1.0.0-beta.2

# set an env var to let the cli know that
# it is running in server environment
ENV HASURA_GRAPHQL_CLI_ENVIRONMENT=server-on-docker

COPY docker-entrypoint.sh /bin/
COPY cli-hasura-linux-amd64 /bin/hasura-cli
RUN chmod +x /bin/hasura-cli

ENTRYPOINT ["docker-entrypoint.sh"]

CMD ["graphql-engine", "serve"]
