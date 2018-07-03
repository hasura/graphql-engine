FROM ubuntu:16.04

ARG docker_ver="17.09.0-ce"
ARG resolver="lts-11.15"
ARG stack_ver="1.7.1"

# Install GNU make, curl, git and docker client. Required to build the server
RUN apt-get -y update \
    && apt-get install -y make curl git \
    && curl -Lo /tmp/docker-${docker_ver}.tgz https://download.docker.com/linux/static/stable/x86_64/docker-${docker_ver}.tgz \
    && tar -xz -C /tmp -f /tmp/docker-${docker_ver}.tgz \
    && mv /tmp/docker/* /usr/bin \
    && curl -sL https://github.com/commercialhaskell/stack/releases/download/v${stack_ver}/stack-${stack_ver}-linux-x86_64.tar.gz \
       | tar xz --wildcards --strip-components=1 -C /usr/local/bin '*/stack' \
    && apt-get -y auto-remove \
    && apt-get -y clean \
    && rm -rf /var/lib/apt/lists/*
