# https://www.andreadiotallevi.com/blog/how-to-create-a-production-image-for-a-node-typescript-app-using-docker-multi-stage-builds

FROM node:18-alpine AS builder
WORKDIR /app
COPY . .
RUN npm ci
RUN npx tsc

FROM node:18-alpine
ENV NODE_ENV=production
RUN apk add --no-cache tini
WORKDIR /app
COPY package.json .
COPY package-lock.json .
RUN npm ci --production
COPY --from=builder ./app/dist ./dist
COPY --from=builder ./app/src/schema.graphql ./dist
EXPOSE 3000
ENTRYPOINT [  "/sbin/tini", "--", "node", "dist/main.js"  ]
