FROM golang:1.10

# setup the working directory
WORKDIR /go/src/github.com/hasura/graphql-engine/cli

RUN ["go", "get", "-u", "github.com/golang/dep/cmd/dep"]
RUN ["go", "get", "github.com/mitchellh/gox"]
RUN ["go", "get", "github.com/hasura/go-bindata/go-bindata"]

ENV HASURA_GRAPHQL_TEST_ENDPOINT=http://172.17.0.4:8080
