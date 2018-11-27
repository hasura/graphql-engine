FROM node:carbon as builder
ENV NODE_ENV=PRODUCTION
WORKDIR /app
COPY package.json ./
RUN npm install --production
COPY . .
RUN npm run build

FROM node:8-alpine
RUN npm -g install serve

WORKDIR /app
COPY --from=builder /app/build .
CMD ["serve", "-p", "8080"]