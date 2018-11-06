#! /bin/bash
docker run -d -p 8080:8080 \
       hasura/graphql-engine:v1.0.0-alpha28 \
       graphql-engine \
       --database-url postgres://username:password@hostname:port/dbname \
       serve --enable-console
