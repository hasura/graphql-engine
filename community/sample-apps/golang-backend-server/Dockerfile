# syntax=docker/dockerfile:1

# Not production ready

##
## Build
##
FROM golang:1.18.4-buster AS build

WORKDIR /app

COPY go.mod ./
COPY go.sum ./
RUN go mod download

COPY . .

RUN go build -o /golang-backend-server

##
## Deploy
##
FROM gcr.io/distroless/base-debian10

WORKDIR /

COPY --from=build /golang-backend-server /golang-backend-server

EXPOSE 3000

USER nonroot:nonroot

ENTRYPOINT ["/golang-backend-server"]