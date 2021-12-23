FROM ubuntu:16.04

ARG docker_ver="17.09.0-ce"
ARG upx_version="3.94"
ARG gcloud_version="207.0.0"
ARG hub_version="2.14.2"

RUN apt-get -y update \
    && apt-get install -y curl make xz-utils git python \
    && curl -Lo /tmp/docker-${docker_ver}.tgz https://download.docker.com/linux/static/stable/x86_64/docker-${docker_ver}.tgz \
    && tar -xz -C /tmp -f /tmp/docker-${docker_ver}.tgz \
    && mv /tmp/docker/* /usr/bin \
    && curl -Lo /tmp/upx-${upx_version}.tar.xz https://github.com/upx/upx/releases/download/v${upx_version}/upx-${upx_version}-amd64_linux.tar.xz \
    && xz -d -c /tmp/upx-${upx_version}.tar.xz \
       | tar -xOf - upx-${upx_version}-amd64_linux/upx > /bin/upx \
    && chmod a+x /bin/upx \
    && curl -Lo /tmp/gcloud-${gcloud_version}.tar.gz https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-sdk-${gcloud_version}-linux-x86_64.tar.gz \
    && tar -xzf /tmp/gcloud-${gcloud_version}.tar.gz -C /usr/local \
    && /usr/local/google-cloud-sdk/install.sh \
    && curl -Lo /tmp/hub-linux-amd64-${hub_version}.tgz  https://github.com/github/hub/releases/download/v${hub_version}/hub-linux-amd64-${hub_version}.tgz \
    && tar -xz -C /tmp -f /tmp/hub-linux-amd64-${hub_version}.tgz \
    && mv /tmp/hub-linux-amd64-${hub_version}/bin/hub /usr/bin/ \
    && apt-get -y purge curl \
    && apt-get -y auto-remove \
    && apt-get -y clean \
    && rm -rf /var/lib/apt/lists/* \
    && rm -rf /usr/share/doc/ \
    && rm -rf /usr/share/man/ \
    && rm -rf /usr/share/locale/

ENV PATH "/usr/local/google-cloud-sdk/bin:$PATH"
