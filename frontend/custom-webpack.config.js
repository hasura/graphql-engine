const { composePlugins, withNx, withWeb } = require('@nrwl/webpack');
// We can't' have react fast refresh because of circular dependancies
// That is also why the current fast refresh is flacky
//const { withReact } = require('@nrwl/react');
const util = require('util');
const withConsoleTweaks = require('./tools/webpack/withConsoleTweaks');
const withDevAssetLoader = require('./tools/webpack/withDevAssetLoader');
const withCircularDependencyPlugin = require('./tools/webpack/withCircularDependencyPlugin');

module.exports = composePlugins(
  // Nx plugins for webpack.
  withNx(),
  // Replace this with withReact for fast refresh once we are able to use it
  withWeb(),
  withConsoleTweaks(),
  withDevAssetLoader()

  /*
  withCircularDependencyPlugin({
    shouldLogEveryCircularDependency: false,
  })

   */
);
