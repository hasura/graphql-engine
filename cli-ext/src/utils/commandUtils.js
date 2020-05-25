const fs = require('fs');

const OUTPUT_FILE_FLAG = 'output-file';
const INPUT_FILE_FLAG = 'input-file';

const getFlagValue = (args, flagname) => {
  for (let i = args.length - 1; i >= 0; i--) {
    if (args[i] === `--${flagname}`) {
      const flagValue = args[i + 1];
      if (!flagValue) {
        throw Error(`unexpected value for flag ${flagname}`);
      } else {
        return flagValue;
      }
    }
  }
};

const getInputPayload = (args) => {
  const inputFilePath = getFlagValue(args, INPUT_FILE_FLAG);
  const payloadString = fs.readFileSync(inputFilePath, 'utf8');
  return JSON.parse(payloadString);
};

module.exports = {
  getInputPayload,
  getFlagValue,
  OUTPUT_FILE_FLAG,
  INPUT_FILE_FLAG,
};
