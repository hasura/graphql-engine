const colors = require('colors/safe');
const {cli} = require('cli-ux');

const log  = (message, color) => {
  if (color) {
    console.log(colors[color](message));
  } else {
    console.log(message);
  }
};

const spinnerStart = message => {
  cli.action.start(message);
};

const spinnerStop = () => {
  cli.action.stop(colors.green('Done!'));
};

module.exports = {
  log,
  spinnerStop,
  spinnerStart,
};

