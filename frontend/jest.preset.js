const nxPreset = require('@nrwl/jest/preset').default;

process.env.TZ = 'UTC';

module.exports = {
  ...nxPreset,
  moduleNameMapper: {
    '\\.(css|less|scss|sass|jpg|ico|jpeg|png|gif|eot|otf|webp|svg|ttf|woff|woff2|mp4|webm|wav|mp3|m4a|aac|oga)$':
      'jest-transform-stub',
  },
  globals: {
    __DEV__: true,
    CONSOLE_ASSET_VERSION: Date.now().toString(),
    'process.hrtime': () => null,
    __DEVELOPMENT__: true,
    __CLIENT__: true,
    __SERVER__: true,
    __DISABLE_SSR__: true,
    __DEVTOOLS__: true,
    socket: true,
    webpackIsomorphicTools: true,
    __env: {
      consoleType: 'oss',
    },
    window: {
      __env: {
        nodeEnv: 'development',
        serverVersion: 'v1.0.0', // FIXME : moving this to the above __env block seem to be breaking some existing tests
      },
    },
  },
};
