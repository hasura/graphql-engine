FROM node:8
# update npm
RUN npm install -g npm@5

ARG docker_ver="17.09.0-ce"

# install dependencies
RUN apt-get update && apt-get install -y \
    netcat \
    libpq5 \
    libgtk2.0-0 \
    libnotify-dev \
    libgconf-2-4 \
    libnss3 \
    libxss1 \
    libasound2 \
    xvfb \
    && curl -Lo /tmp/docker-${docker_ver}.tgz https://download.docker.com/linux/static/stable/x86_64/docker-${docker_ver}.tgz \
    && tar -xz -C /tmp -f /tmp/docker-${docker_ver}.tgz \
    && mv /tmp/docker/* /usr/bin \
    && rm -rf /var/lib/apt/lists/*
