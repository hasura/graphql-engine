const { createGlobPatternsForDependencies } = require('@nrwl/react/tailwind');
const { join } = require('path');
const tailwindConfig = require('../../tailwind.config.js');

module.exports = {
  content: [
    join(
      __dirname,
      '{src,pages,components}/**/*!(*.stories|*.spec).{ts,tsx,html}'
    ),
    ...createGlobPatternsForDependencies(
      __dirname,
      '/**/!(*.stories|*.spec).{ts,tsx,jsx,js,html}'
    ),
  ],
  ...tailwindConfig,
};
