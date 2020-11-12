FROM debian:buster-20200720-slim

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

RUN apt-get update \
  && apt-get install -y ca-certificates libkrb5-3 libpq5 libnuma1
  && apt-get -y auto-remove \
  && apt-get -y clean \
  && rm -rf /var/lib/apt/lists/* \
  && rm -rf /usr/share/doc/ \
  && rm -rf /usr/share/man/ \
  && rm -rf /usr/share/locale/

COPY tmp/graphql-engine /bin/
COPY tmp/pg_dump /bin/
COPY tmp/srv/* /srv

CMD ["graphql-engine", "serve"]
