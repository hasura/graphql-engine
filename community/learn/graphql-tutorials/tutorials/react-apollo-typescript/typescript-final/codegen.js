module.exports = {
    "schema": [
        {
            "https://learn.hasura.io/graphql": {
                "headers": {
                    "x-hasura-admin-secret": ""
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
            ]
        },
        "./graphql.schema.json": {
          "plugins": [
                "introspection"
          ]
        }
    }
};
