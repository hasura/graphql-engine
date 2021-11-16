FROM hasura/graphql-engine-base:b8d72f22801dc01d2f70d3013f0d88f2981ba98bb4f2e4c1bb5d7204deba71c0

COPY rootfs/graphql-engine /bin/
COPY rootfs/pg_dump /bin/
COPY rootfs/srv/ /srv/

CMD ["graphql-engine", "serve"]
