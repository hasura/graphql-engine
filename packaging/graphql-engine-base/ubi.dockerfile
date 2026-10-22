# DATE VERSION: 2026-10-22
# Modify the above date version (YYYY-MM-DD) if you want to rebuild the image

FROM registry.access.redhat.com/ubi9-minimal:9.8-1788939036 as pg_dump_source

ARG TARGETPLATFORM

RUN set -ex; \
    if [ "$TARGETPLATFORM" = "linux/arm64" ]; then \
      rpm -i https://download.postgresql.org/pub/repos/yum/reporpms/EL-9-aarch64/pgdg-redhat-repo-latest.noarch.rpm; \
    else \
      rpm -i https://download.postgresql.org/pub/repos/yum/reporpms/EL-9-x86_64/pgdg-redhat-repo-latest.noarch.rpm; \
    fi; \
    microdnf install -y postgresql17-server

FROM registry.access.redhat.com/ubi9-minimal:9.8-1788939036

ARG TARGETPLATFORM

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

# eMudhra compromised intermediates can chain through either of these roots.
# UBI does not have a confirmed fixed package floor, so persist explicit distrust
# and regenerate the PEM bundles used by HGE and native clients.
# This also distrusts otherwise legitimate chains relying solely on these roots.
RUN set -ex; \
  microdnf update -y ca-certificates; \
  microdnf install -y ca-certificates p11-kit-trust; \
  blocklist=/etc/pki/ca-trust/source/blocklist; \
  mkdir -p "$blocklist"; \
  trust extract --overwrite --format=pem-bundle \
    --filter='pkcs11:object=Comodo%20AAA%20Services%20root;type=cert' \
    "$blocklist/Comodo_AAA_Services_root.pem"; \
  test -s "$blocklist/Comodo_AAA_Services_root.pem"; \
  trust extract --overwrite --format=pem-bundle \
    --filter='pkcs11:object=emSign%20ECC%20Root%20CA%20-%20G3;type=cert' \
    "$blocklist/emSign_ECC_Root_CA_-_G3.pem"; \
  test -s "$blocklist/emSign_ECC_Root_CA_-_G3.pem"; \
  update-ca-trust extract; \
  microdnf clean all

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
  microdnf install -y krb5-libs libpq numactl-libs; \
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
