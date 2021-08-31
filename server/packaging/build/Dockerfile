FROM hasura/graphql-engine-base:1de77086f006c31cb428c07f320f381d3512dd9884cfc0ee75df6138139fdf0c

COPY rootfs/graphql-engine /bin/
COPY rootfs/pg_dump /bin/
COPY rootfs/srv/ /srv/

CMD ["graphql-engine", "serve"]
