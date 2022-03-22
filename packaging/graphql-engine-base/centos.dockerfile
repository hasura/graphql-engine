# DATE VERSION: 2022-03-10
# Modify the above date version (YYYY-MM-DD) if you want to rebuild the image for security updates
FROM quay.io/centos/centos:stream8

# TARGETPLATFORM is automatically set up by docker buildx based on the platform we are targetting for
ARG TARGETPLATFORM

RUN groupadd -g 1001 hasura && useradd -m -u 1001 -g hasura hasura

# Dependencies taken from: https://github.com/0x777/hge-arm-dockerfiles/blob/master/hge.df
# Below are the CentOS libraries matching these apt/Debian ones:
# libpq5 libkrb5-3 libnuma1 ca-certificates (ca-certificates are preinstalled)
RUN yum update -y \
  && yum install -y dnf \
  && dnf upgrade \
  && dnf -qy module disable postgresql \
  && dnf install -y \
  krb5-libs \
  numactl-libs \
  libstdc++ \
  && case "$TARGETPLATFORM" in \
  "linux/amd64") \
    dnf install -y https://download.postgresql.org/pub/repos/yum/reporpms/EL-8-x86_64/pgdg-redhat-repo-latest.noarch.rpm \
    && dnf install -y postgresql13-devel \
    ;; \
  "linux/arm64") \
    dnf install -y https://download.postgresql.org/pub/repos/yum/reporpms/EL-8-aarch64/pgdg-redhat-repo-latest.noarch.rpm \
    # TODO: remove --nogpgcheck after resolving https://www.postgresql.org/message-id/flat/CAA77xwWaf_ZhaTe6qS0O5vrJjx5pJs07GYipn7ZCbMrXfeVqTA%40mail.gmail.com
    && dnf install -y --nogpgcheck postgresql13-devel \
    # TODO: remove following command after removing --nogpgcheck above
    # The following file seem to cause installation issue with packages in the next steps
    # Hence this file is removed here
    && rm -rf /etc/yum.repos.d/pgdg-redhat-all.repo \
    ;; \
  esac

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
