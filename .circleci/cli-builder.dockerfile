FROM golang:1.13

ARG upx_version="3.94"

# install go dependencies
RUN	go get github.com/mitchellh/gox \
    && go get github.com/hasura/go-bindata/go-bindata \
    && go get github.com/tcnksm/ghr

# install UPX, netcat and brotli
RUN apt-get update && apt-get install -y \
    xz-utils netcat libpq5 postgresql-client git cmake pkgconf \
    && curl -Lo /tmp/upx-${upx_version}.tar.xz https://github.com/upx/upx/releases/download/v${upx_version}/upx-${upx_version}-amd64_linux.tar.xz \
    && xz -d -c /tmp/upx-${upx_version}.tar.xz \
       | tar -xOf - upx-${upx_version}-amd64_linux/upx > /bin/upx \
    && chmod a+x /bin/upx \
    && git clone https://github.com/google/brotli.git && cd brotli && mkdir out && cd out && ../configure-cmake \
    && make && make test && make install && ldconfig \
    && apt-get -y auto-remove \
    && rm -rf /var/lib/apt/lists/*
