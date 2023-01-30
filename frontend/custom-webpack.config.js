const { merge } = require('webpack-merge');
const util = require('util');
const webpack = require('webpack');

// un comment this to test out the circular deps
const CircularDependencyPlugin = require('circular-dependency-plugin');
const log = value =>
  console.log(
    util.inspect(value, { showHidden: false, depth: null, colors: true })
  );

const shouldLogEveryCircularDep = false;
let numCyclesDetected = 0;
let filteredCircleDeps = 0;
const depCircleFilter = undefined;

let storedPaths = [];
const shouldLogMostCommonPaths = false;

module.exports = (config, context) => {
  const output = merge(config, {
    output: {
      publicPath: 'auto',
    },
    plugins: [
      new webpack.DefinePlugin({
        __CLIENT__: 'true',
        __SERVER__: false,
        __DEVELOPMENT__: true,
        __DEVTOOLS__: true, // <-------- DISABLE redux-devtools HERE
        CONSOLE_ASSET_VERSION: Date.now().toString(),
      }),
      // un comment this to test out the circular deps. Left here since it can be tricky to configure

      new CircularDependencyPlugin({
        exclude: /node_modules/,
        failOnError: false,
        onStart({ compilation }) {
          numCyclesDetected = 0;
          filteredCircleDeps = 0;
          storedPaths = [];
        },
        onDetected({
          // `paths` will be an Array of the relative module paths that make up the cycle
          paths: cyclePaths,
          compilation,
        }) {
          numCyclesDetected++;
          storedPaths = [...storedPaths, ...cyclePaths];
          const err = new Error(
            `Circular dependency detected!\n * ${cyclePaths.join('\n → ')}`
          );
          if (!shouldLogEveryCircularDep) {
            return;
          }

          if (!depCircleFilter) {
            compilation.warnings.push(err);
            return;
          }

          if (cyclePaths.some(path => path.includes(depCircleFilter))) {
            filteredCircleDeps++;
            compilation.warnings.push(err);
          }
        },
        onEnd({ compilation }) {
          const warns = Array.from(
            Array(Math.round(numCyclesDetected / 100)).keys()
          )
            .map(it => '!')
            .join('');
          compilation.warnings.push(
            new Error(
              `Detected ${numCyclesDetected} circular dependency ` + warns
            )
          );

          if (depCircleFilter) {
            const filterWarns = Array.from(
              Array(Math.round(filteredCircleDeps / 100)).keys()
            )
              .map(it => '!')
              .join('');
            compilation.warnings.push(
              new Error(
                `Detected ${filteredCircleDeps} circular dependency only for the filter "${depCircleFilter}" ${filterWarns}`
              )
            );
          }

          if (shouldLogMostCommonPaths) {
            const topTenPaths = storedPaths
              .sort(
                (a, b) =>
                  storedPaths.filter(v => v === a).length -
                  storedPaths.filter(v => v === b).length
              )
              .slice(0, 10);

            compilation.warnings.push(
              new Error(
                `Here are the top 10 files in the loops :\n${topTenPaths.join(
                  '\n → '
                )}`
              )
            );
          }
        },
        cwd: process.cwd(),
      }),
    ],
    module: {
      rules: [
        /*
        Rule taken from the old codebase
        => Do we still need the version naming ?

         */
        {
          test: /\.svg(\?v=\d+\.\d+\.\d+)?$/,
          use: [
            {
              loader: 'url-loader',
              options: { limit: 10000, mimetype: 'image/svg+xml' },
            },
          ],
        },
      ],
    },
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
      },
    },
  });
  return output;
};
