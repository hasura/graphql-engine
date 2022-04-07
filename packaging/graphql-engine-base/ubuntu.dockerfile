# DATE VERSION: 2022-04-07
# Modify the above date version (YYYY-MM-DD) if you want to rebuild the image for security updates
FROM ubuntu:20.04

# TARGETPLATFORM is automatically set up by docker buildx based on the platform we are targetting for
ARG TARGETPLATFORM

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

RUN groupadd -g 1001 hasura && useradd -m -u 1001 -g hasura hasura

RUN apt-get update \
  && apt-get install -y gnupg2 curl apt-transport-https \
  && apt-get update \
  && apt-get install -y ca-certificates libkrb5-3 libpq5 libnuma1 unixodbc-dev libmariadb-dev-compat mariadb-client-10.3

RUN if [ "$TARGETPLATFORM" = "linux/amd64" ] ; then \
      curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add - \
      && curl https://packages.microsoft.com/config/ubuntu/20.04/prod.list > /etc/apt/sources.list.d/mssql-release.list \
      && apt-get update \
      && ACCEPT_EULA=Y apt-get install -y msodbcsql17 ; \
    fi

# Cleanup unwanted files and packages
RUN apt-get -y remove curl gnupg2 \
  && apt-get -y auto-remove \
  && apt-get -y clean \
  && rm -rf /var/lib/apt/lists/* \
  && rm -rf /usr/share/doc/ \
  && rm -rf /usr/share/man/ \
  && rm -rf /usr/share/locale/