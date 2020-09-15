# Don't update this without updating the
# packager imager of graphql-engine
FROM haskell:8.10.2-stretch

ARG docker_ver="17.09.0-ce"
ARG postgres_ver="12"
ARG node_ver="12.x"

# Install GNU make, curl, git and docker client. Required to build the server
RUN apt-get -y update \
    && apt-get -y install curl gnupg2 \
    && echo "deb http://apt.postgresql.org/pub/repos/apt/ stretch-pgdg main" > /etc/apt/sources.list.d/pgdg.list \
    && curl -s https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
    && apt-get -y update \
    && apt-get install -y \
         g++ gcc git gnupg libc6-dev libffi-dev libgmp-dev libkrb5-dev \
         libpq-dev libssl-dev make netcat postgresql-client-${postgres_ver} \
         postgresql-client-common python3 python3-pip upx xz-utils zlib1g-dev \
    && curl -sL https://deb.nodesource.com/setup_${node_ver} | bash - \
    && apt-get install -y nodejs \
    && curl -Lo /tmp/docker-${docker_ver}.tgz https://download.docker.com/linux/static/stable/x86_64/docker-${docker_ver}.tgz \
    && tar -xz -C /tmp -f /tmp/docker-${docker_ver}.tgz \
    && mv /tmp/docker/* /usr/bin \
    && apt-get -y purge curl \
    && apt-get -y auto-remove \
    && apt-get -y clean \
    && rm -rf /var/lib/apt/lists/* \
    && rm -rf /usr/share/doc/ \
    && rm -rf /usr/share/man/ \
    && rm -rf /usr/share/locale/

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8
