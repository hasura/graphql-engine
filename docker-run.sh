#! /bin/bash
docker run -p 8080:8080 \
       hasuranightly/raven:8df5234 \
       raven \
       --database-url postgres://username:password@hostname:port/dbname \
       serve
