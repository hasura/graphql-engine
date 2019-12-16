import "regenerator-runtime/runtime";
const scaffoldTest = require('./scaffold');
const sdlTest =  require('./sdl');

const runTests = async () => {
  await scaffoldTest();
  await sdlTest();
};

runTests();
