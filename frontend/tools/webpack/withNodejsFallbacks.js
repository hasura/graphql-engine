const { merge } = require('webpack-merge');

module.exports =
  () =>
  (config, { options, context }) => {
    return merge(config, {
      resolve: {
        fallback: {
          /*
        Used by :
        openapi-to-graphql and it's deps (graphql-upload > fs-capacitor)
        no real polyfill exists, so this turns it into an empty implementation
         */
          fs: false,
          /*
        Used by :
        openapi-to-graphql and it's deps (graphql-upload > fs-capacitor)
         */
          os: require.resolve('os-browserify/browser'),

          /*
        Used by :
        openapi-to-graphql and it's deps (swagger2openapi)
         */
          http: require.resolve('stream-http'),
          /*
        Used by :
        @graphql-codegen/typescript and it's deps (@graphql-codegen/visitor-plugin-common && parse-filepath)
        => one usage is found, so we have to check if the usage is still relevant
         */
          path: require.resolve('path-browserify'),
          /*
        Used by :
        jsonwebtoken deps (jwa && jws)
        => we already have an equivalent in the codebases that don't depend on it,jwt-decode.
           Might be worth using only the latter
         */
          crypto: require.resolve('crypto-browserify'),
          /*
        Used by :
        jsonwebtoken deps (jwa && jws)
        @graphql-tools/merge => dependanci of graphiql & graphql-codegen/core, a package upgrade might fix it
         */
          util: require.resolve('util/'),
          /*
        Used by :
        jsonwebtoken deps (jwa && jws)
         */
          stream: require.resolve('stream-browserify'),
          https: require.resolve('https-browserify'),
          url: require.resolve('url/'),
        },
      },
    });
  };
