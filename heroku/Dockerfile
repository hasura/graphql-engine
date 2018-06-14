FROM hasuranightly/raven:c9a69e1

CMD raven \
    --database-url $DATABASE_URL \
    serve \
    --server-port $PORT \
    --cors-domain "http://localhost:9695"
