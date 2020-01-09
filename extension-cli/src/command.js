import "regenerator-runtime/runtime";
import {
  sdl,
  actionsCodegen
} from './services';
import version from './utils/version';
import fs from 'fs';
import { getFlagValue, OUTPUT_FILE_FLAG } from './utils/commandUtils';

const commandArgs = process.argv;
const outputFilePath = getFlagValue(commandArgs, OUTPUT_FILE_FLAG);

const logOutput = (log) => {
  try {
    fs.writeFile(outputFilePath, log, 'utf8', () => {
      console.log(JSON.stringify({
        success: true,
        output_file_path: outputFilePath
      }));
    });
  } catch (e) {
    console.log(e);
    console.error({
      error: `could not write output to "${outputFilePath}"`
    })
  }
};

const handleArgs = () => {
  const rootArg = commandArgs[2];
  switch(rootArg) {
    case 'sdl':
      const sdlSubCommands = commandArgs.slice(3);
      return sdl(sdlSubCommands);
    case 'actions-codegen':
      const actionCodegenSubCommands = commandArgs.slice(3);
      return actionsCodegen(actionCodegenSubCommands);
    case 'version':
      const versionSubCommands = commandArgs.slice(3);
      return version(versionSubCommands);
    default:
      return;
  }
}

try {
  let cliResponse = handleArgs();
  if (cliResponse.constructor.name === 'Promise') {
    cliResponse.then(r => {
      logOutput(r);
    }).catch(e => {
      console.error(e);
      process.exit(1);
    })
  } else {
    logOutput(r);
  }
} catch (e) {
  console.error(e);
  process.exit(1);
}
