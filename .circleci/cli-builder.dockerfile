FROM golang:1.16

ARG upx_version="3.96"

# install go dependencies
RUN	go get github.com/mitchellh/gox \
    && go get github.com/tcnksm/ghr

# install UPX, netcat and gcloud cli
RUN apt-get update && apt-get install -y \
    xz-utils netcat libpq5 postgresql-client jq zip \
    && curl -s https://packages.microsoft.com/config/debian/9/prod.list > /etc/apt/sources.list.d/mssql-release.list \
    && curl -s https://packages.microsoft.com/keys/microsoft.asc | apt-key add - \
    && echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list \
    && curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key --keyring /usr/share/keyrings/cloud.google.gpg add - \
    && apt-get update \
    && ACCEPT_EULA=Y apt-get install -y ca-certificates libkrb5-3 libpq5 libnuma1 unixodbc-dev msodbcsql17 apt-transport-https gnupg google-cloud-sdk hub \
    && curl -Lo /tmp/upx-${upx_version}.tar.xz https://github.com/upx/upx/releases/download/v${upx_version}/upx-${upx_version}-amd64_linux.tar.xz \
    && xz -d -c /tmp/upx-${upx_version}.tar.xz \
       | tar -xOf - upx-${upx_version}-amd64_linux/upx > /bin/upx \
    && chmod a+x /bin/upx \
    && apt-get -y auto-remove \
    && rm -rf /var/lib/apt/lists/*
