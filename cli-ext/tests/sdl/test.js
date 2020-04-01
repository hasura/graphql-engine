const { toPayload, fromPayload } = require('./payload');

const { handlePayload: toHandler  } = require('../../build/services/sdl/to/handler');
const { handlePayload: fromHandler } = require('../../build/services/sdl/from/handler');

const test = async () => {

  const toResponse = toHandler(toPayload);

  if (
    toResponse.status === 200
  ) {
    console.log('✓ Conversion from metadata to SDL passed');
  } else {
    console.log('✘ Conversion from metadata to SDL failed');
    console.log(toResponse);
    process.exit(1);
  }

  const fromResponse = fromHandler(fromPayload);
  if (
    fromResponse.status === 200 
  ) {
    console.log('✓ Conversion from SDL to metadata passed');
  } else {
    console.log('✘ Conversion from SDL to metadata failed');
    console.log(fromResponse);
    process.exit(1);
  }

  return Promise.resolve()

}

module.exports = test;
