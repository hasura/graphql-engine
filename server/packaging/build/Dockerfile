FROM debian:buster-20200720-slim

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

RUN apt-get update \
  && apt-get install -y ca-certificates libkrb5-3 libpq5 libnuma1

COPY tmp/graphql-engine /bin/
COPY tmp/pg_dump /bin/
COPY tmp/srv/* /srv

CMD ["graphql-engine", "serve"]
