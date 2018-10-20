FROM ubuntu:16.04

ARG docker_ver="17.09.0-ce"
ARG resolver="lts-11.15"
ARG stack_ver="1.7.1"

# Install GNU make, curl, git and docker client. Required to build the server
RUN apt-get -y update \
    && apt-get install -y curl g++ gcc libc6-dev libpq-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg upx netcat python3 python3-pip \
    && curl -Lo /tmp/docker-${docker_ver}.tgz https://download.docker.com/linux/static/stable/x86_64/docker-${docker_ver}.tgz \
    && tar -xz -C /tmp -f /tmp/docker-${docker_ver}.tgz \
    && mv /tmp/docker/* /usr/bin \
    && curl -sL https://github.com/commercialhaskell/stack/releases/download/v${stack_ver}/stack-${stack_ver}-linux-x86_64.tar.gz \
       | tar xz --wildcards --strip-components=1 -C /usr/local/bin '*/stack' \
    && stack --resolver ${resolver} setup \
    && apt-get -y purge curl \
    && apt-get -y auto-remove \
    && apt-get -y clean \
    && rm -rf /var/lib/apt/lists/* \
    && rm -rf /usr/share/doc/ \
    && rm -rf /usr/share/man/ \
    && rm -rf /usr/share/locale/
