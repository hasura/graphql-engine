#! /bin/bash
docker run -p 8080:8080 \
       hasura/graphql-engine:latest \
       raven \
       --database-url postgres://username:password@hostname:port/dbname \
       serve
