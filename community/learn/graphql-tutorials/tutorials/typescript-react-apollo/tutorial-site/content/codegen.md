---
title: "GraphQL -> Typescript Codegen"
metaTitle: "GraphQL Code Generator Setup | GraphQL React Apollo Typescript Tutorial"
metaDescription: "You will learn how to configure GraphQL Code Generator to auto generate types for all the GraphQL queries, mutations and subscriptions"
---

import GithubLink from "../src/GithubLink.js";

Apollo Client has been configured. Now everytime we use GraphQL queries, we need the relevant types to be mapped to the query and response. Let's automate that process using `graphql-code-generator`.

[GraphQL Code Generator](https://github.com/dotansimha/graphql-code-generator) is a CLI tool that generates code out of your GraphQL schema. 
It analyzes and parses GraphQL syntax in order to output a wide variety of code formats, typings and even components. In this tutorial, we will make use of the automatic typings for all the GraphQL portions of the app.

## Install graphql-code-generator

```bash
yarn add @graphql-codegen/cli @graphql-codegen/introspection @graphql-codegen/typescript @graphql-codegen/typescript-operations @graphql-codegen/typescript-react-apollo
```

After the modules are installed, we will configure the code generator specifying what we would like to generate: 

Create a new file `codegen.js` in the root directory and copy the contents below into this file.

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/typescript-react-apollo/app-final/codegen.js" text="codegen.js" />

```javascript
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
```

The configuration points to `https://learn.hasura.io/graphql` schema where our GraphQL API lives. It is configured to generate two files: `graphql.tsx` to output the types and `graphql.schema.json`, to output the result of introspection.

Let's add a new script to `package.json` to autogenerate these files.

Head to package.json and add this script

```json
  "scripts": {
    "start": "react-scripts start",
    "build": "react-scripts build",
    "test": "react-scripts test",
-   "eject": "react-scripts eject"
+   "eject": "react-scripts eject",
+   "generate": "graphql-codegen --config codegen.js"
  },
```

Head to [GraphiQL](https://learn.hasura.io/graphql/graphiql) to obtain the Authorization token. 

Now run the following command to generate the types

```bash
AUTH_TOKEN=xxxxx yarn generate --watch
```

*Note*: Replace xxxxx with the Auth Token you copied from GraphiQL. The Auth Token shouldn't have `Bearer`

This generates two files:

1. `graphql.schema.json` - Introspection result of the GraphQL server goes into this file in JSON format.
2. `src/generated/graphql.tsx` - It will generate the necessary types for mapping GraphQL queries to Typescript.

We will import the generated GraphQL types in different parts of the tutorial and see how it helps with type safety and how much hand written code is avoided.

This script is running in `watch` mode and hence will autogenerate types for any queries/mutations/subscriptions that is included later in the tutorial. In case you are resuming after sometime, be sure to run this command to keep your generated types updated.


