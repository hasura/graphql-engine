FROM scratch
COPY rootfs/ /
ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

RUN addgroup -g 1000 hasura \
    && adduser -u 1000 -s /bin/sh -S -G hasura hasura

USER 1000

CMD ["graphql-engine", "serve"]
