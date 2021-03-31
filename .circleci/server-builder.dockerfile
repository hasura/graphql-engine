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

FROM haskell:8.8-stretch
# ^ Required to build 8.10.2 below; 

ARG docker_ver="17.09.0-ce"
ARG postgres_ver="12"
ARG node_ver="12.x"

# Install GNU make, curl, git and docker client. Required to build the server
RUN apt-get -y update     \
    && apt-get -y install curl gnupg2 apt-transport-https     \
    && echo "deb http://apt.postgresql.org/pub/repos/apt/ stretch-pgdg main" > /etc/apt/sources.list.d/pgdg.list     \
    && curl -s https://packages.microsoft.com/config/debian/9/prod.list > /etc/apt/sources.list.d/mssql-release.list     \
    && curl -s https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -     \
    && curl -s https://packages.microsoft.com/keys/microsoft.asc | apt-key add -     \
    && apt-get -y update     \
    && apt-get install -y          \
        g++ gcc git gnupg libc6-dev libffi-dev libgmp-dev libkrb5-dev          \
        libpq-dev libssl-dev make netcat postgresql-client-${postgres_ver}          \
        postgresql-client-common python3 python3-pip upx xz-utils zlib1g-dev          \
        unixodbc-dev freetds-dev     \
    && ACCEPT_EULA=Y apt-get -y install msodbcsql17     \
    && curl -sL https://deb.nodesource.com/setup_${node_ver} | bash -     \
    && apt-get install -y nodejs     \
    && curl -Lo /tmp/docker-${docker_ver}.tgz https://download.docker.com/linux/static/stable/x86_64/docker-${docker_ver}.tgz     \
    && tar -xz -C /tmp -f /tmp/docker-${docker_ver}.tgz     \
    && mv /tmp/docker/* /usr/bin     \
    && apt-get -y purge curl     \
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

# No idea why this is suddenly necessary... (https://app.circleci.com/pipelines/github/hasura/graphql-engine-mono/6783/workflows/1e1489c3-7517-4257-85c5-aa67e66206ef/jobs/144747 )
RUN git config --global user.email "you@example.com" \
    && git config --global user.name "Your Name" 

# Matt's changes, with some trivial conflicts fixed for 8.10.1:
COPY 0001-mpickering-s-memory-RTS-changes-backported-to-8.10.1.patch /

RUN apt -y update \
    && apt -y install build-essential git autoconf python3 libgmp-dev libncurses-dev \
    && cabal v2-update \
    && cabal v2-install alex happy-1.19.12 \
    && git clone https://gitlab.haskell.org/ghc/ghc.git \
    && cd ghc \
    && git checkout ghc-8.10.1-release \
    && git submodule update --init --recursive \
    && git am -3k /0001-mpickering-s-memory-RTS-changes-backported-to-8.10.1.patch \
    && ./boot && ./configure && make -j4 && make install \
    && cd / \
    && rm -rf ghc \
    && rm -rf /opt/ghc/  /root/.cabal/store
