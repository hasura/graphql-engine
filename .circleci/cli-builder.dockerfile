FROM golang:1.10

# install go dependencies
RUN	go get github.com/golang/dep/cmd/dep
RUN go get github.com/mitchellh/gox
RUN go get github.com/hasura/go-bindata/go-bindata

# install UPX
RUN apt-get update && apt-get install -y \
    xz-utils \
    && rm -rf /var/lib/apt/lists/*
ADD https://github.com/upx/upx/releases/download/v3.94/upx-3.94-amd64_linux.tar.xz /usr/local
RUN xz -d -c /usr/local/upx-3.94-amd64_linux.tar.xz | \
    tar -xOf - upx-3.94-amd64_linux/upx > /bin/upx && \
    chmod a+x /bin/upx
