//  enable runtime transpilation to use ES6/7 in node

const fs = require('fs');

const babelrc = fs.readFileSync('.babelrc');
let config;

try {
  config = JSON.parse(babelrc);
} catch (err) {
  console.error('==>     ERROR: Error parsing your .babelrc.');
  console.error(err);
}

require('babel-core/register')(config);
