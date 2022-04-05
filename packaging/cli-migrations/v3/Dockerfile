ARG BASE_IMAGE

FROM ${BASE_IMAGE}

# yum update uses --nobest option to tackle https://github.com/hasura/graphql-engine-mono/issues/4096
RUN { apt-get update && apt-get install -y netcat; } \
  || { yum update -y --nobest && yum install -y nc; }

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
COPY hasura-cli /bin/hasura-cli

RUN chmod +x /bin/hasura-cli 

# set an env var to let the cli know that
# it is running in server environment
ENV HASURA_GRAPHQL_CLI_ENVIRONMENT=server-on-docker

ENTRYPOINT ["docker-entrypoint.sh"]

CMD ["graphql-engine", "serve"]
