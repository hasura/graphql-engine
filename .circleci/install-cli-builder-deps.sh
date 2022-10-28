#!/usr/bin/env bash

set -eo pipefail

apt-get update && apt-get install -y \
    xz-utils netcat libpq5 postgresql-client jq zip \
    && curl -s https://packages.microsoft.com/config/ubuntu/20.04/prod.list > /etc/apt/sources.list.d/mssql-release.list \
    && curl -s https://packages.microsoft.com/keys/microsoft.asc | apt-key add - \
    && echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list \
    && curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key --keyring /usr/share/keyrings/cloud.google.gpg add - \
    && apt-get update \
    && ACCEPT_EULA=Y apt-get install -y ca-certificates libkrb5-3 libpq5 libnuma1 unixodbc-dev msodbcsql17 msodbcsql18 apt-transport-https gnupg google-cloud-sdk hub \
    && apt-get -y auto-remove \
    && rm -rf /var/lib/apt/lists/*


# install go1.16

wget https://golang.org/dl/go1.16.3.linux-amd64.tar.gz
rm -rf /usr/local/go && tar -C /usr/local -xzf go1.16.3.linux-amd64.tar.gz
export PATH=$PATH:/usr/local/go/bin
go version
