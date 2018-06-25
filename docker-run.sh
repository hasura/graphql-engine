#! /bin/bash
docker run -p 8080:8080 \
       hasura/graphql-engine:190d78e \
       raven \
       --database-url postgres://username:password@hostname:port/dbname \
       serve
