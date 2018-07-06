FROM node:8
# update npm
RUN npm install -g npm@5

# install netcat
RUN apt-get update && apt-get install -y \
    netcat libpq5 \
    && rm -rf /var/lib/apt/lists/*