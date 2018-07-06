FROM node:8
# update npm
RUN npm install -g npm@5

# install netcat
RUN apt-get update && apt-get install -y \
    netcat libpq5 \
    && rm -rf /var/lib/apt/lists/*

# install gcloud sdk
ADD https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-sdk-207.0.0-linux-x86_64.tar.gz /tmp
RUN tar -xzf /tmp/google-cloud-sdk-207.0.0-linux-x86_64.tar.gz -C /usr/local
RUN /usr/local/google-cloud-sdk/install.sh
ENV PATH "/usr/local/google-cloud-sdk/bin:$PATH"

