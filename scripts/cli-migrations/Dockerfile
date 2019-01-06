FROM hasura/graphql-engine:v1.0.0-alpha34

COPY docker-entrypoint.sh /bin/
COPY cli-hasura-linux-amd64 /bin/hasura-cli
RUN chmod +x /bin/hasura-cli

ENTRYPOINT ["docker-entrypoint.sh"]

CMD ["graphql-engine", "serve"]
