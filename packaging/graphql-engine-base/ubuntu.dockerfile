# DATE VERSION: 2023-10-21
# Modify the above date version (YYYY-MM-DD) if you want to rebuild the image

FROM ubuntu:jammy-20231004

### NOTE! Shared libraries here need to be kept in sync with `server-builder.dockerfile`!

# TARGETPLATFORM is automatically set up by docker buildx based on the platform we are targetting for
ARG TARGETPLATFORM

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

RUN set -ex; \
    groupadd -g 1001 hasura; \
    useradd -m -u 1001 -g hasura hasura

RUN set -ex; \
    apt-get update; \
    apt-get upgrade -y; \
    # basic deps
    apt-get install -y apt-transport-https ca-certificates curl gnupg2 lsb-release;  \
    # deps needed for graphql-engine
    apt-get install -y libkrb5-3 libpq5 libnuma1 unixodbc-dev; \
    # deps needed for cli-migrations
    apt-get install -y netcat

RUN set -ex; \
    curl -fsS "https://packages.microsoft.com/config/ubuntu/$(lsb_release -rs)/prod.list" > /etc/apt/sources.list.d/mssql-release.list; \
    curl -fsS 'https://packages.microsoft.com/keys/microsoft.asc' | apt-key add -; \
    apt-get update; \
    ACCEPT_EULA=Y apt-get install -y msodbcsql18; \
    if [ "$TARGETPLATFORM" = "linux/amd64" ]; then \
      # Support the old version of the driver too, where possible.
      # v17 is only supported on amd64.
      ACCEPT_EULA=Y apt-get -y install msodbcsql17; \
    fi

# Install pg_dump
RUN set -ex; \
    curl -s https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -; \
    echo 'deb http://apt.postgresql.org/pub/repos/apt jammy-pgdg main' > /etc/apt/sources.list.d/pgdg.list; \
    apt-get -y update; \
    apt-get install -y postgresql-client-15; \
    # delete all pg tools except pg_dump to keep the image minimal
    find /usr/bin -name 'pg*' -not -path '/usr/bin/pg_dump' -delete

# Cleanup unwanted files and packages
# Note: curl is not removed, it's required to support health checks
RUN set -ex; \
    apt-get -y remove gnupg2; \
    apt-get -y auto-remove; \
    apt-get -y clean; \
    rm -rf /var/lib/apt/lists/* /usr/share/doc/ /usr/share/man/ /usr/share/locale/
