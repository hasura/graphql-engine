# Don't update this without updating the
# packager imager of graphql-engine
FROM phadej/ghc:8.10.1-stretch
# TODO https://github.com/haskell/docker-haskell/issues/17
#FROM haskell:8.10.1

ARG docker_ver="17.09.0-ce"
ARG postgres_ver="12"
ARG node_ver="8.x"

# Install GNU make, curl, git and docker client. Required to build the server
RUN apt-get -y update \
    && apt-get -y install curl gnupg2 \
    && echo "deb http://apt.postgresql.org/pub/repos/apt/ stretch-pgdg main" > /etc/apt/sources.list.d/pgdg.list \
    && curl -s https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
    && apt-get -y update \
    && apt-get install -y g++ gcc libc6-dev libpq-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg upx netcat python3 python3-pip postgresql-client-${postgres_ver} postgresql-client-common \
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
