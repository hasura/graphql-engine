const handler = require('./handler');

const command = (subCommands) => {
  const rootInput = subCommands[0];
  const payload = JSON.parse(rootInput);
  return handler(payload);
};

module.exports = command;