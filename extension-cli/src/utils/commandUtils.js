export const OUTPUT_FILE_FLAG = 'output-file';

export const getFlagValue = (args, flagname) => {
  for (var i = args.length - 1; i >= 0; i--) {
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
