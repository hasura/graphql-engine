const toCommand = require('./to/command');
const fromCommand = require('./from/command');

const command = (subCommands) => {
  const rootSubCommand = subCommands[0];
  switch(rootSubCommand) {
    case 'to':
      const toSubCommands = subCommands.slice(1);
      return toCommand(toSubCommands)
    case 'from':
      const fromSubCommands = subCommands.slice(1);
      return fromCommand(fromSubCommands);
    default:
      return;
  }
};

module.exports = command;