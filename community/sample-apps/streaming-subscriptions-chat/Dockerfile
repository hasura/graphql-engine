FROM node:16 as builder
ENV NODE_ENV=PRODUCTION
WORKDIR /app
COPY package.json ./
COPY package-lock.json ./
RUN npm install
COPY . .
RUN npm run build

FROM node:16-alpine
RUN npm -g install serve

WORKDIR /app
COPY --from=builder /app/build .
CMD ["serve", "-s", "-p", "3000"]