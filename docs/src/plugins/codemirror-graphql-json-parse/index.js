// graphiql 1.5.1 lints the variables editor with codemirror-graphql's CommonJS
// build, whose ES5-transpiled JSONSyntaxError breaks `instanceof`. The linter
// then rethrows on malformed JSON and crashes the whole docs page. Point those
// requires at the ESM build, which keeps a native class, so bad variables show
// an inline lint marker instead.
module.exports = function codemirrorGraphqlJsonParse() {
  return {
    name: 'codemirror-graphql-json-parse',
    configureWebpack() {
      const webpack = require('webpack');
      const esmJsonParse = require.resolve('codemirror-graphql/esm/utils/jsonParse.js');
      return {
        plugins: [
          new webpack.NormalModuleReplacementPlugin(/[\\/]utils[\\/]jsonParse$/, resource => {
            if (/codemirror-graphql[\\/](?!esm[\\/])/.test(resource.context)) {
              resource.request = esmJsonParse;
            }
          }),
        ],
      };
    },
  };
};
