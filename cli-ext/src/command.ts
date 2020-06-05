import 'regenerator-runtime/runtime';

const fs = require('fs');
const { sdl, actionsCodegen } = require('./services');
const { getFlagValue, OUTPUT_FILE_FLAG } = require('./utils/commandUtils');

const commandArgs = process.argv;
const outputFilePath = getFlagValue(commandArgs, OUTPUT_FILE_FLAG);

const logOutput = (log: any) => {
  try {
    fs.writeFile(outputFilePath, log, 'utf8', () => {
      console.log(
        JSON.stringify({
          success: true,
          output_file_path: outputFilePath,
        }),
      );
    });
  } catch (e) {
    console.error(`could not write output to "${outputFilePath}"`);
    process.exit(1);
  }
};

const handleArgs = () => {
  const rootArg = commandArgs[2];
  switch (rootArg) {
    case 'sdl':
      const sdlSubCommands = commandArgs.slice(3);
      return sdl(sdlSubCommands);
    case 'actions-codegen':
      const actionCodegenSubCommands = commandArgs.slice(3);
      return actionsCodegen(actionCodegenSubCommands);
    default:
  }
};

try {
  const cliResponse = handleArgs();
  if (cliResponse.error) {
    throw Error(cliResponse.error);
  }
  if (cliResponse.constructor.name === 'Promise') {
    cliResponse
      .then((r: { error?: string }) => {
        if (r.error) {
          throw Error(r.error);
        }
        logOutput(JSON.stringify(r));
      })
      .catch((e: Error) => {
        console.error(e);
        process.exit(1);
      });
  } else {
    logOutput(JSON.stringify(cliResponse));
  }
} catch (e) {
  if (e) {
    if (e.error) {
      console.error(e.error);
    } else {
      console.error(e.message ? e.message : e);
    }
  }
  process.exit(1);
}
