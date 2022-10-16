# DATE VERSION: 2022-09-21
# Modify the above date version (YYYY-MM-DD) if you want to rebuild the image for security updates
FROM debian:bullseye-20220912-slim

### NOTE! Shared libraries here need to be kept in sync with `server-builder.dockerfile`!

# TARGETPLATFORM is automatically set up by docker buildx based on the platform we are targetting for
ARG TARGETPLATFORM

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

RUN set -ex; \
    groupadd -g 1001 hasura; \
    useradd -m -u 1001 -g hasura hasura

RUN set -ex; \
    apt-get update; \
    apt-get install -y apt-transport-https curl gnupg2; \
    apt-get update; \
    apt-get install -y ca-certificates libkrb5-3 libpq5 libssl1.1 libnuma1 unixodbc-dev libmariadb-dev-compat mariadb-client

RUN if [ "$TARGETPLATFORM" = "linux/amd64" ] ; then \
      curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add -; \
      curl https://packages.microsoft.com/config/debian/11/prod.list > /etc/apt/sources.list.d/mssql-release.list; \
      apt-get update; \
      ACCEPT_EULA=Y apt-get install -y msodbcsql17; \
    fi

# Install pg_dump
RUN set -ex; \
    curl -s https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -; \
    echo 'deb http://apt.postgresql.org/pub/repos/apt bullseye-pgdg main' > /etc/apt/sources.list.d/pgdg.list; \
    apt-get -y update; \
    apt-get install -y postgresql-client; \
    # delete all pg tools except pg_dump to keep the image minimal
    find /usr/bin -name 'pg*' -not -path '/usr/bin/pg_dump' -delete

# Cleanup unwanted files and packages
RUN set -ex; \
    apt-get -y remove curl gnupg2; \
    apt-get -y auto-remove; \
    apt-get -y clean; \
    rm -rf /var/lib/apt/lists/* /usr/share/doc/ /usr/share/man/ /usr/share/locale/
