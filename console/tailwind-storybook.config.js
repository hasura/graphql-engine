const tailwindConfig = require('./tailwind.config.js');

module.exports = {
  ...tailwindConfig,
  important: '.hasura-tailwind-on',
};
