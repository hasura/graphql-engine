# DATE VERSION: 2022-05-16
# Modify the above date version (YYYY-MM-DD) if you want to rebuild the image for security updates
FROM quay.io/centos/centos@sha256:e0dd15bcd9aaeebec1c74236e9764cd18e198f35631d72e07039c9a2aa66fa4e

# TARGETPLATFORM is automatically set up by docker buildx based on the platform we are targetting for
ARG TARGETPLATFORM

RUN groupadd -g 1001 hasura && useradd -m -u 1001 -g hasura hasura

# Dependencies taken from: https://github.com/0x777/hge-arm-dockerfiles/blob/master/hge.df
# Below are the CentOS libraries matching these apt/Debian ones:
# libpq5 libkrb5-3 libnuma1 ca-certificates (ca-certificates are preinstalled)
#
# yum/dnf update/upgrade uses --nobest option to tackle https://github.com/hasura/graphql-engine-mono/issues/4096
RUN yum update -y --nobest \
  && yum install -y dnf \
  && dnf upgrade -y --nobest \
  && dnf -qy module disable postgresql \
  && dnf install -y \
  krb5-libs \
  numactl-libs \
  libstdc++ \
  && case "$TARGETPLATFORM" in \
  "linux/amd64") \
    dnf install -y https://download.postgresql.org/pub/repos/yum/reporpms/EL-8-x86_64/pgdg-redhat-repo-latest.noarch.rpm \
    ;; \
  "linux/arm64") \
    dnf install -y https://download.postgresql.org/pub/repos/yum/reporpms/EL-8-aarch64/pgdg-redhat-repo-latest.noarch.rpm \
    ;; \
  esac \
  && dnf install -y postgresql14-devel \
  # delete all pg tools except pg_dump to keep the image minimal
  && find /usr/bin -name 'pg*' -not -path '/usr/bin/pg_dump' -delete

# msodbcsql17
RUN yum remove unixODBC-utf16 unixODBC-utf16-devel \
  && yum install -y unixODBC-devel \
  && if [ "$TARGETPLATFORM" = "linux/amd64" ] ; then \
      curl https://packages.microsoft.com/config/rhel/8/prod.repo > /etc/yum.repos.d/mssql-release.repo \
      && ACCEPT_EULA=Y yum install -y msodbcsql17 ; \
    fi

# mysql
RUN dnf install -y mariadb-connector-c pcre-devel \
  && ln -s /usr/lib64/libpcre.so /usr/lib64/libpcre.so.3

RUN yum clean all && rm -rf /var/cache/yum
