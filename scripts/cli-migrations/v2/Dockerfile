FROM hasura/graphql-engine:v1.1.1

RUN wget http://ftp.us.debian.org/debian/pool/main/g/gcc-10/libstdc++6_10-20200312-2_amd64.deb -O lib.deb \
    && busybox dpkg-deb -x lib.deb / \
    && rm lib.deb

# set an env var to let the cli know that
# it is running in server environment
ENV HASURA_GRAPHQL_CLI_ENVIRONMENT=server-on-docker
ENV HASURA_GRAPHQL_SHOW_UPDATE_NOTIFICATION=false

COPY docker-entrypoint.sh /bin/
COPY cli-hasura-linux-amd64 /bin/hasura-cli
COPY manifest.yaml /tmp/manifest.yaml
RUN chmod +x /bin/hasura-cli \
    && hasura-cli plugins install cli-ext --manifest-file /tmp/manifest.yaml \
    && rm /tmp/manifest.yaml

ENTRYPOINT ["docker-entrypoint.sh"]

CMD ["graphql-engine", "serve"]
