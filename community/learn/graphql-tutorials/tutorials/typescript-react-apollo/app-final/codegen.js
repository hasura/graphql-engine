module.exports = {
    "schema": [
        {
            "https://learn.hasura.io/graphql": {
                "headers": {
                    "Authorization": "Bearer " + process.env.AUTH_TOKEN
                }
            }
        }
    ],
    "documents": [
        "./src/**/*.tsx",
        "./src/**/*.ts"
    ],
    "overwrite": true,
    "generates": {
        "./src/generated/graphql.tsx": {
            "plugins": [
                "typescript",
                "typescript-operations",
                "typescript-react-apollo"
            ],
            "config": {
              "skipTypename": false,
              "withHooks": true,
              "withHOC": false,
              "withComponent": false
            }
        },
        "./graphql.schema.json": {
          "plugins": [
                "introspection"
          ]
        }
    }
};
