FROM hasura/graphql-engine as gql
FROM scratch
COPY rootfs/ /
COPY --from=gql /srv /srv
CMD ["graphql-engine", "serve"]
