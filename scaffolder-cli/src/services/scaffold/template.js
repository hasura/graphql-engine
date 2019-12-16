const templaters = require('../../templaters');

const getFrameworkScaffold = async (framework, mutationSdl, typesSdl) => {

  if (!templaters[framework]) {
    return {
      error: 'could not find the given scaffold'
    }
  }

  let scaffoldFiles = templaters[framework](mutationSdl, typesSdl);
  if (scaffoldFiles && scaffoldFiles.constructor.name === 'Promise') {
    scaffoldFiles = await scaffoldFiles;
  }

  return {
    files: scaffoldFiles
  }

};

module.exports = {
  getFrameworkScaffold 
};
