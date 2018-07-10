FROM node:8
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
    && rm -rf /var/lib/apt/lists/*