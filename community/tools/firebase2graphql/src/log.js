const colors = require('colors/safe');
const {cli} = require('cli-ux');

const shouldLog = process.env.F2G_LOG;

const log  = (message, color) => {
  if (shouldLog !== '0') {
    if (color) {
      console.log(colors[color](message));
    } else {
      console.log(message);
    }
  }
};

const spinnerStart = message => {
  if (shouldLog !== '0') {
    cli.action.start(message);
  }
};

const spinnerStop = () => {
  if (shouldLog !== '0') {
    cli.action.stop(colors.green('Done!'));
  }
};

const spinnerStopColorless = message => {
  if (shouldLog !== '0') {
    cli.action.stop(message);
  }
};

module.exports = {
  log,
  spinnerStop,
  spinnerStart,
  spinnerStopColorless,
};

