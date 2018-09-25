const {cli} = require('cli-ux');
const {log} = require('./log');
const colors = require('colors/safe');

module.exports = message => {
  cli.action.stop(colors.red('Error!'));
  console.log('');
  log(message, 'red');
  process.exit(1);
};
