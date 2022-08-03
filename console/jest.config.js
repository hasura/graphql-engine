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
    window: {
      __env: {
        nodeEnv: 'development',
        serverVersion: 'v1.0.0',
      },
    },
  },
};
