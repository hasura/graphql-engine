const handler = require('./handler');

const command = (subCommands) => {
  const rootInput = subCommands[0];
  const payload = JSON.parse(rootInput);
  const response = handler(payload);
  return response;
};

module.exports = command;