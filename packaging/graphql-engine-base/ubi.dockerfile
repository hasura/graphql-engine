# DATE VERSION: 2026-07-17
# Modify the above date version (YYYY-MM-DD) if you want to rebuild the image

FROM registry.access.redhat.com/ubi10-minimal:10.2-1784094212 AS pg_dump_source

ARG TARGETPLATFORM

RUN set -ex; \
  if [ "$TARGETPLATFORM" = "linux/arm64" ]; then \
  rpm -i https://download.postgresql.org/pub/repos/yum/reporpms/EL-10-aarch64/pgdg-redhat-repo-latest.noarch.rpm; \
  else \
  rpm -i https://download.postgresql.org/pub/repos/yum/reporpms/EL-10-x86_64/pgdg-redhat-repo-latest.noarch.rpm; \
  fi; \
  microdnf install -y postgresql18-server 

FROM registry.access.redhat.com/ubi10-minimal:10.2-1784094212

ARG TARGETPLATFORM

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

RUN set -ex; \
  microdnf install -y shadow-utils; \
  groupadd -g 1001 hasura; \
  useradd -m -u 1001 -g hasura hasura; \
  microdnf remove -y shadow-utils

# Install pg_dump
COPY --from=pg_dump_source /usr/bin/pg_dump /usr/bin/pg_dump

RUN set -ex; \
  # deps needed for graphql-engine
  microdnf install -y krb5-libs libpq numactl-libs; \
  # deps for cli-migrations
  microdnf install -y nc

RUN set -ex; \
  curl -fsS https://packages.microsoft.com/config/rhel/10/prod.repo | tee /etc/yum.repos.d/mssql-release.repo; \
  ACCEPT_EULA=Y microdnf install -y msodbcsql18 unixODBC-devel; \
  if [ "$TARGETPLATFORM" = "linux/amd64" ]; then \
  # Support the old version of the driver too, where possible.
  # v17 is only supported on amd64.
  ACCEPT_EULA=Y microdnf -y install msodbcsql17; \
  fi; \
  microdnf clean all
