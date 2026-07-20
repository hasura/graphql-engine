FROM ubuntu:noble-20260610
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
    apt-get install -y libkrb5-3 libpq5 libnuma1 postgresql-common; \
    # deps needed for cli-migrations
    apt-get install -y netcat-traditional

RUN set -ex; \
    curl -sSL -O https://packages.microsoft.com/config/ubuntu/$(lsb_release -rs)/packages-microsoft-prod.deb; \
    dpkg -i packages-microsoft-prod.deb; \
    rm packages-microsoft-prod.deb; \
    apt-get update; \
    ACCEPT_EULA=Y apt-get install -y unixodbc-dev msodbcsql18; \
    if [ "$TARGETPLATFORM" = "linux/amd64" ]; then \
      # Support the old version of the driver too, where possible.
      # v17 is only supported on amd64.
      ACCEPT_EULA=Y apt-get -y install msodbcsql17; \
    fi

# Install pg_dump
# NOTE!: this should always track the latest supported version
# You must also update:
#   - server-builder.dockerfile in this repo
#   - the 'hasura/lux' repo, for AMI building scripts
RUN set -ex; \
    /usr/share/postgresql-common/pgdg/apt.postgresql.org.sh -y; \
    apt-get -y update; \
    apt-get install -y postgresql-client-18; \
    # delete all pg tools except pg_dump to keep the image minimal
    find /usr/bin -name 'pg*' -not -path '/usr/bin/pg_dump' -delete

# Cleanup unwanted files and packages
# Note: curl is not removed, it's required to support health checks
RUN set -ex; \
    apt-get -y remove gnupg2; \
    apt-get -y auto-remove; \
    apt-get -y clean; \
    rm -rf /var/lib/apt/lists/* /usr/share/doc/ /usr/share/man/ /usr/share/locale/

# Remove the default snakeoil private key and certificate so they are not left
# in the image (installed as a side effect of the ssl-cert dependency)
RUN rm -f /etc/ssl/private/ssl-cert-snakeoil.key /etc/ssl/certs/ssl-cert-snakeoil.pem
