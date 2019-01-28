const {cli} = require('cli-ux');

module.exports = message => {
  cli.action.stop('Error');
  console.log(message);
  process.exit(1);
};
