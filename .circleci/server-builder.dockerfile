# NOTE!: If you update this file you need to:
#
# - take the hash of the new file with `sha256sum server-builder.dockerfile`
# - update `&server_builder_image` with this new hash (see TODO there)
# - if any system libraries were added, or the base image was changed, be sure
#   to make the same modification to the packager dockerfile at: server/packaging/build/Dockerfile
#
# The builder image will then get rebuilt automatically, and used in the rest of the build pipeline.
#
# TODO we should combine this with  server/packaging/build/Dockerfile using a multi-stage build:
#      https://docs.docker.com/develop/develop-images/multistage-build/
#
# NOTE: this is symlinked from mono .circleci to oss-.circleci so that it is
#       visible to OSS users, since it's at least a good reference

# FROM haskell:8.10.2-buster
FROM haskell:8.8.3-buster
# ^ Required to build 8.10.2 below;

ARG docker_ver="19.03.13"
ARG postgres_ver="13"
ARG node_ver="12.x"

# Install GNU make, curl, git and docker client. Required to build the server
RUN apt-get -y update     \
    && apt-get -y install curl gnupg2 apt-transport-https     \
    && echo "deb http://apt.postgresql.org/pub/repos/apt/ buster-pgdg main" > /etc/apt/sources.list.d/pgdg.list     \
    && curl -s https://packages.microsoft.com/config/debian/9/prod.list > /etc/apt/sources.list.d/mssql-release.list     \
    && curl -s https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -     \
    && curl -s https://packages.microsoft.com/keys/microsoft.asc | apt-key add -     \
    && apt-get -y update     \
    && apt-get install -y          \
        g++ gcc git gnupg libc6-dev libffi-dev libgmp-dev libkrb5-dev          \
        libpq-dev libssl-dev make netcat postgresql-client-${postgres_ver}          \
        postgresql-client-common python3 python3-pip upx xz-utils zlib1g-dev          \
        unixodbc-dev freetds-dev     \
        default-libmysqlclient-dev libghc-pcre-light-dev libkrb5-dev \
    && ACCEPT_EULA=Y apt-get -y install msodbcsql17     \
    && curl -sL https://deb.nodesource.com/setup_${node_ver} | bash -     \
    && apt-get install -y nodejs     \
    && curl -Lo /tmp/docker-${docker_ver}.tgz https://download.docker.com/linux/static/stable/x86_64/docker-${docker_ver}.tgz     \
    && tar -xz -C /tmp -f /tmp/docker-${docker_ver}.tgz     \
    && mv /tmp/docker/* /usr/bin     \
    && apt-get -y auto-remove     \
    && apt-get -y clean     \
    && rm -rf /var/lib/apt/lists/*     \
    && rm -rf /usr/share/doc/     \
    && rm -rf /usr/share/man/     \
    && rm -rf /usr/share/locale/

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

# Pull in haskell-related paths; make sure to set the same path for bash interactivity
ENV PATH="/root/.cabal/bin:${PATH}:/opt/ghc/bin"
RUN echo "export PATH=${PATH}" >> /root/.bashrc

# ###################################
# Building Matt's GHC fork with:
#   https://gitlab.haskell.org/mpickering/ghc/-/commits/wip/fd-decay-factor
#
# To revert, remove everything below this line, and change back the base image above.

RUN apt -y update \
    && apt -y install build-essential git autoconf python3 libgmp-dev libncurses-dev \
    && cabal v2-update \
    && cabal v2-install alex happy-1.19.12 \
    && git clone https://gitlab.haskell.org/ghc/ghc.git \
    && cd ghc \
    && git checkout ghc-8.10.2-release \
    && git submodule update --init --recursive \
    && git remote add mpickering https://gitlab.haskell.org/mpickering/ghc \
    && git fetch mpickering \
    && git merge 962b00e7fbf75913 \
    && ./boot && ./configure && make -j4 && make install \
    && cd / \
    && rm -rf ghc \
    && rm -rf /opt/ghc/  /root/.cabal/store

# adding this install step here to make use of build cache on docker build
# if not it'll trigger a rebuild of GHC

# if the man directories are missing, postgresql-client fails to install in debian
RUN mkdir -p /usr/share/man/man{1,7} && apt-get -y update

RUN apt-get -y update \
    && apt-get -y install pgbouncer jq postgresql-client-13 default-mysql-client
