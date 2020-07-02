#!/usr/bin/env bash

set -evo pipefail

upx_version="3.94"

# install UPX and netcat
apt-get update && apt-get install -y \
    xz-utils netcat libpq5 postgresql-client curl git \
    && curl -Lo /tmp/upx-${upx_version}.tar.xz https://github.com/upx/upx/releases/download/v${upx_version}/upx-${upx_version}-amd64_linux.tar.xz \
    && xz -d -c /tmp/upx-${upx_version}.tar.xz \
       | tar -xOf - upx-${upx_version}-amd64_linux/upx > /bin/upx \
    && chmod a+x /bin/upx \
    && apt-get -y auto-remove \
    && rm -rf /var/lib/apt/lists/*

# setup go
curl -LO https://golang.org/dl/go1.13.12.linux-amd64.tar.gz
tar -xvf go1.13.12.linux-amd64.tar.gz -C /usr/local
chown -R root:root /usr/local/go
export PATH=$PATH:/usr/local/go/bin
mkdir -p $HOME/go/{bin,src}
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin

# install go dependencies
go get github.com/mitchellh/gox \
    && go get github.com/hasura/go-bindata/go-bindata \
    && go get github.com/tcnksm/ghr

# start a postgres container
docker run -p 5432:5432 -e "POSTGRES_USER=gql_test" -e "POSTGRES_DB=gql_test" -d postgres:10
