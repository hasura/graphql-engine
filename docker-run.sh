#! /bin/bash
docker run -p 8080:8080 \
       hasura/graphql-engine:4d2d2ca \
       raven \
       --database-url postgres://username:password@hostname:port/dbname \
       serve
