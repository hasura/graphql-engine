FROM node:10-alpine
COPY package.json package-lock.json ./
RUN npm ci
COPY index.js .
CMD ["node", "node_modules/.bin/micro"]
