const { getInputPayload } = require('../../utils/commandUtils');
const handler = require('./handler');

const command = (subCommands) => {
  const rootInput = subCommands[0];
  const payload = getInputPayload(subCommands);
  return handler(payload);
};

module.exports = command;
