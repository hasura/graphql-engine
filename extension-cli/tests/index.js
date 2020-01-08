import "regenerator-runtime/runtime";
const actionsCodegen = require('./actions-codegen');
const sdlTest =  require('./sdl');

const runTests = async () => {
  await actionsCodegen();
  await sdlTest();
};

runTests();
