const { getActionScaffold } = require('../../build/services/scaffold/scaffold');
const { samplePayload } = require('./payload');

const test = async () => {

  let actionScaffold;
  try {
    const scaffolds = await getActionScaffold(samplePayload);
    if (scaffolds) {
      console.log('✓ Scaffold test passed');
    } else {
      console.log('✘ Scaffold test failed');
      console.log('Received empty scaffold');
      process.exit(1);
    }
  } catch (e) {
    console.log('✘ Scaffold test failed');
    console.error(e);
    process.exit(1);
  }

};

module.exports = test;
