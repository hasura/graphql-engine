const { getInputPayload } = require('../../utils/commandUtils');
const handler = require('./handler');

const command = (subCommands) => {
  const payload = getInputPayload(subCommands);
  return handler(payload);
};

module.exports = command;
