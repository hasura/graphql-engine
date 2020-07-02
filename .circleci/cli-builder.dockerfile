FROM golang:1.13

ARG upx_version="3.94"
ARG docker_ver="17.09.0-ce"

# install go dependencies
RUN	go get github.com/mitchellh/gox \
    && go get github.com/hasura/go-bindata/go-bindata \
    && go get github.com/tcnksm/ghr

# install UPX and netcat
RUN apt-get update && apt-get install -y \
    xz-utils netcat libpq5 postgresql-client \
    && curl -Lo /tmp/docker-${docker_ver}.tgz https://download.docker.com/linux/static/stable/x86_64/docker-${docker_ver}.tgz \
    && tar -xz -C /tmp -f /tmp/docker-${docker_ver}.tgz \
    && mv /tmp/docker/* /usr/bin \
    && curl -Lo /tmp/upx-${upx_version}.tar.xz https://github.com/upx/upx/releases/download/v${upx_version}/upx-${upx_version}-amd64_linux.tar.xz \
    && xz -d -c /tmp/upx-${upx_version}.tar.xz \
       | tar -xOf - upx-${upx_version}-amd64_linux/upx > /bin/upx \
    && chmod a+x /bin/upx \
    && apt-get -y auto-remove \
    && rm -rf /var/lib/apt/lists/*
RUN curl -L https://github.com/docker/compose/releases/download/1.25.3/docker-compose-`uname -s`-`uname -m` > /usr/local/bin/docker-compose \ 
    && chmod +x /usr/local/bin/docker-compose