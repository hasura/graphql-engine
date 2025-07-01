# DATE VERSION: 2025-06-30
# Modify the above date version (YYYY-MM-DD) if you want to rebuild the image

FROM registry.access.redhat.com/ubi9-minimal:9.6-1751286687 as pg_dump_source

ARG TARGETPLATFORM

RUN set -ex; \
    if [ "$TARGETPLATFORM" = "linux/arm64" ]; then \
      rpm -i https://download.postgresql.org/pub/repos/yum/reporpms/EL-9-aarch64/pgdg-redhat-repo-latest.noarch.rpm; \
    else \
      rpm -i https://download.postgresql.org/pub/repos/yum/reporpms/EL-9-x86_64/pgdg-redhat-repo-latest.noarch.rpm; \
    fi; \
    microdnf install -y postgresql16-server

FROM registry.access.redhat.com/ubi9-minimal:9.6-1751286687

ARG TARGETPLATFORM

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

RUN set -ex; \
    microdnf install -y shadow-utils; \
    groupadd -g 1001 hasura; \
    useradd -m -u 1001 -g hasura hasura; \
    microdnf remove -y shadow-utils

# Install pg_dump
COPY --from=pg_dump_source /usr/bin/pg_dump /usr/bin/pg_dump

# mysql
# graphql-engine is built on ubuntu, so the binary links to the debian version of pcre so(libpcre.so.3).
# here, we are creating a symlink to libpcre.so.1. we did this in the centos images as well:
# https://github.com/hasura/graphql-engine-mono/blob/02dc61f05e06f46c193be3685abbef9c8535edef/packaging/graphql-engine-base/centos.dockerfile#L45
RUN ln -s /usr/lib64/libpcre.so.1 /usr/lib64/libpcre.so.3

RUN set -ex; \
    # deps needed for graphql-engine
    microdnf install -y krb5-libs libpq-13.20-1.el9_5 numactl-libs; \
    # deps for cli-migrations
    microdnf install -y nc

RUN set -ex; \
    curl -fsS https://packages.microsoft.com/config/rhel/9/prod.repo | tee /etc/yum.repos.d/mssql-release.repo; \
    ACCEPT_EULA=Y microdnf install -y msodbcsql18 unixODBC-devel; \
    if [ "$TARGETPLATFORM" = "linux/amd64" ]; then \
      # Support the old version of the driver too, where possible.
      # v17 is only supported on amd64.
      ACCEPT_EULA=Y microdnf -y install msodbcsql17; \
    fi; \
    microdnf clean all
