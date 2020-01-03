const handler = () => {

  let versionJson; 
  try {
    versionJson = require('../../version.json')
    return versionJson.version;
  } catch (_) {
    return ""
  }
  
};

export default handler;