FROM hasura/graphql-engine:v1.0.0-alpha28

ENV HASURA_GRAPHQL_ENABLE_CONSOLE=true

COPY docker-entrypoint.sh /bin/
COPY cli-hasura-linux-amd64 /bin/hasura-cli
RUN chmod +x /bin/hasura-cli

ENTRYPOINT ["docker-entrypoint.sh"]

CMD ["graphql-engine", "serve"]
