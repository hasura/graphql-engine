#!/usr/bin/env bash

set -eo pipefail

apt-get update && apt-get install -y \
    xz-utils netcat libpq5 postgresql-client jq zip \
    && curl -sSL -O https://packages.microsoft.com/config/ubuntu/$(lsb_release -rs)/packages-microsoft-prod.deb \
    && dpkg -i packages-microsoft-prod.deb \
    && rm packages-microsoft-prod.deb \
    && echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list \
    && curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | gpg --dearmor -o /usr/share/keyrings/cloud.google.gpg \
    && apt-get update \
    && ACCEPT_EULA=Y apt-get install -y ca-certificates libkrb5-3 libpq5 libnuma1 unixodbc-dev msodbcsql17 msodbcsql18 apt-transport-https gnupg google-cloud-sdk hub

apt-get -y auto-remove \
    && rm -rf /var/lib/apt/lists/*

# install go1.26
wget https://golang.org/dl/go1.26.4.linux-amd64.tar.gz
rm -rf /usr/local/go && tar -C /usr/local -xzf go1.26.4.linux-amd64.tar.gz
export PATH=$PATH:/usr/local/go/bin
go version
