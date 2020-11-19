const handler = require('./handler');
const { getInputPayload } = require('../../../utils/commandUtils');

const command = (subCommands) => {
  const payload = getInputPayload(subCommands);
  const response = handler(payload);
  return response;
};

module.exports = command;
