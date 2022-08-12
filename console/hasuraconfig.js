module.exports = {
  hmrPort: parseInt(process.env.PORT, 10) + 1 || 3001,
  hmrHost: process.env.HOST || '127.0.0.1',
  appHost: '0.0.0.0',
  port: { development: process.env.PORT, production: 8080 },
  assetsPrefix: '/rstatic',
  webpackPrefix: '/rstatic/dist/',
  appPrefix: '/rapp',
};
