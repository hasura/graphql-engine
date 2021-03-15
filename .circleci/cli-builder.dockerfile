FROM golang:1.14

ARG upx_version="3.96"

# install go dependencies
RUN	go get github.com/mitchellh/gox \
    && go get github.com/hasura/go-bindata/go-bindata \
    && go get github.com/tcnksm/ghr

# install UPX and netcat
RUN apt-get update && apt-get install -y \
    xz-utils netcat libpq5 postgresql-client \
    && curl -s https://packages.microsoft.com/config/debian/9/prod.list > /etc/apt/sources.list.d/mssql-release.list \
    && curl -s https://packages.microsoft.com/keys/microsoft.asc | apt-key add - \
    && apt-get update \
    && ACCEPT_EULA=Y apt-get install -y ca-certificates libkrb5-3 libpq5 libnuma1 unixodbc-dev msodbcsql17 \
    && curl -Lo /tmp/upx-${upx_version}.tar.xz https://github.com/upx/upx/releases/download/v${upx_version}/upx-${upx_version}-amd64_linux.tar.xz \
    && xz -d -c /tmp/upx-${upx_version}.tar.xz \
       | tar -xOf - upx-${upx_version}-amd64_linux/upx > /bin/upx \
    && chmod a+x /bin/upx \
    && apt-get -y auto-remove \
    && rm -rf /var/lib/apt/lists/*
