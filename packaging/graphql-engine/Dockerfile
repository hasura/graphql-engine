ARG BASE_IMAGE
FROM ${BASE_IMAGE}

ARG HGE_BINARY_NAME=graphql-engine
ENV HGE_BINARY ${HGE_BINARY_NAME}

COPY rootfs/bin/ /bin/
COPY rootfs/srv/ /srv/

HEALTHCHECK --start-period=10s CMD curl -f http://localhost:8080/healthz || exit 1

CMD "${HGE_BINARY}" serve
