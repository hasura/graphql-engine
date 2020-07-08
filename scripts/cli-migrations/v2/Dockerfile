FROM hasura/haskell-docker-packager:20190731 as packager
WORKDIR /tmp
RUN apt-get update && apt-get download libstdc++6

FROM hasura/graphql-engine:v1.3.0-beta.4

# install libstdc++6 from .deb file
COPY --from=packager /tmp/libstdc++6* .
RUN busybox dpkg-deb -x libstdc++6*.deb / \
    && rm libstdc++6*.deb

# set an env var to let the cli know that
# update notification is disabled
ENV HASURA_GRAPHQL_SHOW_UPDATE_NOTIFICATION=false

COPY docker-entrypoint.sh /bin/
COPY cli-hasura-linux-amd64 /bin/hasura-cli
COPY manifest.yaml /tmp/manifest.yaml
COPY cli-ext-hasura-linux.tar.gz /tmp/cli-ext/cli-ext-hasura-linux.tar.gz
COPY ./hasura-home-dir-tmpl /tmp/hasura-home-directory
RUN chmod +x /bin/hasura-cli 

ENTRYPOINT ["docker-entrypoint.sh"]

CMD ["graphql-engine", "serve"]
