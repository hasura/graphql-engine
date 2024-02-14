ARG BASE_IMAGE
FROM ${BASE_IMAGE}

# When a non-root user without home directory is trying to use the cli-migrations image
# then hasura cli will try to create .hasura folder at the root but would fail due to permissions
# (example: OpenShift runs containers by using a random user-id which is homeless and non-root)
# The following commands give the right permissions for /.hasura folder
RUN mkdir -p /.hasura \
  && chgrp -R 0 /.hasura \
  && chmod -R g=u /.hasura

# set an env var to let the cli know that
# update notification is disabled
ENV HASURA_GRAPHQL_SHOW_UPDATE_NOTIFICATION=false

COPY docker-entrypoint.sh /bin/
COPY --chmod=+x hasura-cli /bin/hasura-cli

# set an env var to let the cli know that
# it is running in server environment
ENV HASURA_GRAPHQL_CLI_ENVIRONMENT=server-on-docker
ARG HGE_BINARY
ENV HGE_BINARY=${HGE_BINARY}

ENTRYPOINT ["docker-entrypoint.sh"]

CMD $HGE_BINARY serve
