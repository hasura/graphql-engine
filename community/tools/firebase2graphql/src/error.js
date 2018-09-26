const {cli} = require('cli-ux');
const {log} = require('./log');
const colors = require('colors/safe');

module.exports = (message, preExitHook) => {
  cli.action.stop(colors.red('Error!'));
  if (preExitHook) {
    preExitHook(message);
  }
  console.log('');
  log(message, 'red');
  process.exit(1);
};
