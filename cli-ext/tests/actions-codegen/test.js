const { getActionsCodegen } = require('../../build/services/actions-codegen/codegen');
const { samplePayload } = require('./payload');

const test = async () => {

  try {
    const codegenFiles = await getActionsCodegen(samplePayload);
    if (codegenFiles) {
      console.log('✓ Actions codegen test passed');
    } else {
      console.log('✘ Actions codegen test failed');
      console.log('Received empty codegen');
      process.exit(1);
    }
  } catch (e) {
    console.log('✘ Actions codegen test failed');
    console.error(e);
    process.exit(1);
  }

};

module.exports = test;
