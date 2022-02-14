ARG BASE_IMAGE

FROM ${BASE_IMAGE}

COPY rootfs/graphql-engine /bin/
COPY rootfs/pg_dump /bin/
COPY rootfs/srv/ /srv/

CMD ["graphql-engine", "serve"]
