# Don't update this without updating the
# packager imager of graphql-engine
FROM debian:stretch-20190228-slim

ARG docker_ver="17.09.0-ce"
ARG resolver="lts-13.20"
ARG stack_ver="1.9.3"
ARG postgres_ver="11"
ARG node_ver="8.x"

# Install GNU make, curl, git and docker client. Required to build the server
RUN apt-get -y update \
    && apt-get -y install curl gnupg2 \
    && echo "deb http://apt.postgresql.org/pub/repos/apt/ stretch-pgdg main" > /etc/apt/sources.list.d/pgdg.list \
    && curl -s https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
    && apt-get -y update \
    && apt-get install -y g++ gcc libc6-dev libpq-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg upx netcat python3 python3-pip postgresql-client-${postgres_ver} postgresql-client-common \
    && curl -sL https://deb.nodesource.com/setup_${node_ver} | bash - \
    && apt-get install -y nodejs \
    && curl -Lo /tmp/docker-${docker_ver}.tgz https://download.docker.com/linux/static/stable/x86_64/docker-${docker_ver}.tgz \
    && tar -xz -C /tmp -f /tmp/docker-${docker_ver}.tgz \
    && mv /tmp/docker/* /usr/bin \
    && curl -sL https://github.com/commercialhaskell/stack/releases/download/v${stack_ver}/stack-${stack_ver}-linux-x86_64.tar.gz \
       | tar xz --wildcards --strip-components=1 -C /usr/local/bin '*/stack' \
    && stack --resolver ${resolver} setup \
    && stack build Cabal-2.4.1.0 \
    && apt-get -y purge curl \
    && apt-get -y auto-remove \
    && apt-get -y clean \
    && rm -rf /var/lib/apt/lists/* \
    && rm -rf /usr/share/doc/ \
    && rm -rf /usr/share/man/ \
    && rm -rf /usr/share/locale/
