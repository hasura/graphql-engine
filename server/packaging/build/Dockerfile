FROM scratch
COPY rootfs/ /
ENV HASURA_GRAPHQL_ENABLE_CONSOLE=true
CMD ["graphql-engine", "serve"]
