import "regenerator-runtime/runtime";
import {
  sdl,
  scaffold
} from './services';

const commandArgs = process.argv;

const handleArgs = () => {
  const rootArg = commandArgs[2];

  switch(rootArg) {
    case 'sdl':
      const sdlSubCommands = commandArgs.slice(3);
      return sdl(sdlSubCommands);
    case 'scaffold':
      const scaffoldSubCommands = commandArgs.slice(3);
      return scaffold(scaffoldSubCommands);
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
