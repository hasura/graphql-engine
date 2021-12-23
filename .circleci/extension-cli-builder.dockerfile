FROM node:12-bullseye

ARG upx_version="3.94"

# install dependencies
RUN apt-get update && apt-get install -y zip xvfb build-essential pkg-config libssl-dev libcurl4-openssl-dev \
    && wget https://github.com/mtrojnar/osslsigncode/releases/download/2.2/osslsigncode-2.2.0.tar.gz \
    && tar -xzf osslsigncode-2.2.0.tar.gz \
    && cd osslsigncode-2.2.0 \
    && ./configure && make && make install && cd .. \
    && rm -rf osslsigncode* \
    && curl -Lo /tmp/upx-${upx_version}.tar.xz https://github.com/upx/upx/releases/download/v${upx_version}/upx-${upx_version}-amd64_linux.tar.xz \
    && xz -d -c /tmp/upx-${upx_version}.tar.xz \
    | tar -xOf - upx-${upx_version}-amd64_linux/upx > /bin/upx \
    && chmod a+x /bin/upx \
    && apt-get -y auto-remove \
    && apt-get -y clean \
    && rm -rf /var/lib/apt/lists/* \
    && rm -rf /usr/share/doc/ \
    && rm -rf /usr/share/man/ \
    && rm -rf /usr/share/locale/