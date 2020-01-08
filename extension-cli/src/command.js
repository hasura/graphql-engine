import "regenerator-runtime/runtime";
import {
  sdl,
  actionsCodegen
} from './services';
import version from './utils/version';

const commandArgs = process.argv;

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
      console.log(r);
    }).catch(e => {
      console.error(e);
      process.exit(1);
    })
  } else {
    console.log(cliResponse);
  }
} catch (e) {
  console.error(e);
  process.exit(1);
}
