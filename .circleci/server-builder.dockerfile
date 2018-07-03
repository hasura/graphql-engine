FROM ubuntu:16.04

ARG VER="17.09.0-ce"

# Install GNU make, curl, git and docker client. Required to build the server
RUN apt-get -y update \
    && apt-get install -y make curl git \
    && curl -L -o /tmp/docker-${VER}.tgz https://download.docker.com/linux/static/stable/x86_64/docker-${VER}.tgz \
    && tar -xz -C /tmp -f /tmp/docker-${VER}.tgz \
    && mv /tmp/docker/* /usr/bin \
    && apt-get -y auto-remove \
    && rm -rf /var/lib/apt/lists/*
