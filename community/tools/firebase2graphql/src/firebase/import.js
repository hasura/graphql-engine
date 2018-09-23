const generateGenericJson = require('./generateGenericJson.js');
const importGenericJson = require('../import/import');

const importData = async (db, url, headers, overwrite) => {
  const validJson = generateGenericJson(db);
  await importGenericJson(validJson, url, headers, overwrite, true);
};

module.exports = importData;