const chalk = require('chalk');

module.exports = function warn(elmJsonPath) {
  const elmJson = require(elmJsonPath);
  if (elmJson.homepage || elmJson.proxy) {
    console.log();
    console.log(chalk.yellow('Warning:'));
    console.log();
    console.log(
      '  Using elm.json for configuring "homepage" and "proxy" is deprecated.'
    );
    console.log('  This feature will be removed in the future versions.');
    console.log();
  }
};
