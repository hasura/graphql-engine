# DATE VERSION: 2022-04-12
# Modify the above date version (YYYY-MM-DD) if you want to rebuild the image for security updates
FROM debian:buster-20210511-slim

# TARGETPLATFORM is automatically set up by docker buildx based on the platform we are targetting for
ARG TARGETPLATFORM

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

RUN groupadd -g 1001 hasura && useradd -m -u 1001 -g hasura hasura

RUN apt-get update \
  && apt-get install -y gnupg2 curl apt-transport-https \
  && apt-get update \
  && apt-get install -y ca-certificates libkrb5-3 libpq5 libnuma1 unixodbc-dev default-libmysqlclient-dev default-mysql-client

RUN if [ "$TARGETPLATFORM" = "linux/amd64" ] ; then \
      curl -s https://packages.microsoft.com/config/debian/9/prod.list > /etc/apt/sources.list.d/mssql-release.list \
      && curl -s https://packages.microsoft.com/keys/microsoft.asc | apt-key add - \
      && apt-get update \
      && ACCEPT_EULA=Y apt-get install -y msodbcsql17 ; \
    fi

# Install pg_dump
RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ buster-pgdg main" > /etc/apt/sources.list.d/pgdg.list \
  && curl -s https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
  && apt-get -y update \
  && apt-get install -y \
    postgresql-client-14 \
  # delete all pg tools except pg_dump to keep the image minimal
  && find /usr/bin -name 'pg*' -not -path '/usr/bin/pg_dump' -delete

# Cleanup unwanted files and packages
RUN apt-get -y remove curl gnupg2 \
  && apt-get -y auto-remove \
  && apt-get -y clean \
  && rm -rf /var/lib/apt/lists/* \
  && rm -rf /usr/share/doc/ \
  && rm -rf /usr/share/man/ \
  && rm -rf /usr/share/locale/