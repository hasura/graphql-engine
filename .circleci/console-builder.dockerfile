FROM node:8

ARG gcloud_version="207.0.0"

# update npm
RUN npm install -g npm@5

# install dependencies
RUN apt-get update && apt-get install -y \
    netcat \
    libpq5 \
    libgtk2.0-0 \
    libnotify-dev \
    libgconf-2-4 \
    libnss3 \
    libxss1 \
    libasound2 \
    xvfb \
    && curl -Lo /tmp/gcloud-${gcloud_version}.tar.gz https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-sdk-${gcloud_version}-linux-x86_64.tar.gz \
    && tar -xzf /tmp/gcloud-${gcloud_version}.tar.gz -C /usr/local \
    && /usr/local/google-cloud-sdk/install.sh \
    && apt-get -y auto-remove \
    && apt-get -y clean \
    && rm -rf /var/lib/apt/lists/* \
    && rm -rf /usr/share/doc/ \
    && rm -rf /usr/share/man/ \
    && rm -rf /usr/share/locale/

ENV PATH "/usr/local/google-cloud-sdk/bin:$PATH"
