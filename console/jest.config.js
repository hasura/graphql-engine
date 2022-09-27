module.exports = {
  roots: ['<rootDir>/src'],
  transform: {
    '^.+\\.(ts|tsx|js|jsx)?$': 'ts-jest',
    '^.+\\.svg$': 'jest-svg-transformer',
  },
  testRegex: '^.+\\.(test|spec).[jt]sx?$',
  setupFilesAfterEnv: ['<rootDir>/src/setupTests.ts'],
  moduleFileExtensions: ['ts', 'tsx', 'js', 'jsx', 'json', 'node'],
  moduleDirectories: ['node_modules', 'src'],
  testEnvironment: 'jsdom',
  moduleNameMapper: {
    '\\.(jpg|ico|jpeg|png|gif|eot|otf|webp|svg|ttf|woff|woff2|mp4|webm|wav|mp3|m4a|aac|oga)$':
      '<rootDir>/src/fileMock.js',
    '^@/(.*)$': '<rootDir>/src/$1',
    '\\.(css|less|scss|sass)$': 'identity-obj-proxy',
  },
  globals: {
    'ts-jest': {
      tsconfig: 'tsconfig.test.json',
    },
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
