# Don't update this without updating the
# packager imager of graphql-engine
FROM debian:stretch-20190228-slim

ARG docker_ver="17.09.0-ce"
ARG resolver="lts-13.20"
ARG stack_ver="2.1.3"
ARG postgres_ver="11"

# Install GNU make, curl, git and docker client. Required to build the server
RUN apt-get -y update \
    && apt-get -y install curl gnupg2 cmake pkgconf sudo \
    && echo "deb http://apt.postgresql.org/pub/repos/apt/ stretch-pgdg main" > /etc/apt/sources.list.d/pgdg.list \
    && curl -s https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
    && apt-get -y update \
    && apt-get install -y g++ gcc libc6-dev libpq-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg upx netcat python3 python3-pip postgresql-client-${postgres_ver} postgresql-client-common \
    && curl -Lo /tmp/docker-${docker_ver}.tgz https://download.docker.com/linux/static/stable/x86_64/docker-${docker_ver}.tgz \
    && tar -xz -C /tmp -f /tmp/docker-${docker_ver}.tgz \
    && mv /tmp/docker/* /usr/bin \
    && git clone https://github.com/google/brotli.git && cd brotli && mkdir out && cd out && ../configure-cmake \
    && make && make test && make install && ldconfig \
    && curl -sL https://github.com/commercialhaskell/stack/releases/download/v${stack_ver}/stack-${stack_ver}-linux-x86_64.tar.gz \
       | tar xz --wildcards --strip-components=1 -C /usr/local/bin '*/stack' \
       # Install Python 3.7 start\
    && apt-get install -y build-essential checkinstall \
    && apt-get install -y libreadline-gplv2-dev libncursesw5-dev libssl-dev \
         libsqlite3-dev tk-dev libgdbm-dev libc6-dev libbz2-dev libffi-dev zlib1g-dev \
    && curl -O https://www.python.org/ftp/python/3.7.4/Python-3.7.4.tar.xz \
    && tar -xf Python-3.7.4.tar.xz \
    && cd Python-3.7.4 \
    && ./configure --enable-optimizations \
    && make -j 4 \
    && make altinstall \
    && python3.7 --version \
    && rm -rf Python-3.7.4 Python-3.7.4.tar.xz \
       # Install Python 3.7 end \
    && stack --resolver ${resolver} setup \
    && stack build Cabal-2.4.1.0 \
    && apt-get -y purge curl \
    && apt-get -y auto-remove \
    && apt-get -y clean \
    && rm -rf /var/lib/apt/lists/* \
    && rm -rf /usr/share/doc/ \
    && rm -rf /usr/share/man/ \
    && rm -rf /usr/share/locale/

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8
