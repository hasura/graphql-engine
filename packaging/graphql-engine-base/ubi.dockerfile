# DATE VERSION: 2026-10-22
# Modify the above date version (YYYY-MM-DD) if you want to rebuild the image

FROM registry.access.redhat.com/ubi10/ubi-minimal:1792457309 AS pg_dump_source

ARG TARGETPLATFORM

RUN set -ex; \
  if [ "$TARGETPLATFORM" = "linux/arm64" ]; then \
  rpm -i https://download.postgresql.org/pub/repos/yum/reporpms/EL-10-aarch64/pgdg-redhat-repo-latest.noarch.rpm; \
  else \
  rpm -i https://download.postgresql.org/pub/repos/yum/reporpms/EL-10-x86_64/pgdg-redhat-repo-latest.noarch.rpm; \
  fi; \
  microdnf install -y postgresql18-server 

FROM registry.access.redhat.com/ubi10/ubi-minimal:1792457309

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
