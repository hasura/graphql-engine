const {spinnerStart, spinnerStop, log} = require('../log');
const generate = require('./generateTables');
const {refineJson} = require('./utils');
const {generateSql, runSql, dropUtilityTables} = require('./sql');
const {trackTables} = require('./track');
const {getInsertOrder, insertData} = require('./insert');
const {createRelationships} = require('./relationships');
const {createTables} = require('./check');
const normalize = require('./normalize');
const generateGenericJson = require('../firebase/generateGenericJson');
const makeSuggestions = require('./suggest');

const importData = async (jsonDb, url, headers, overwrite, level = 1, shouldNormalize) => {
  spinnerStart('Processing Firebase JSON');
  const db = level === 1 ? refineJson(generateGenericJson(jsonDb)) : jsonDb;
  const tables = generate(db);
  const sql = generateSql(tables);
  spinnerStop('Done!');
  spinnerStart('Checking database');
  createTables(tables, url, headers, overwrite, runSql, sql).then(() => {
    spinnerStop('Done!');
    spinnerStart('Tracking tables');
    trackTables(tables, url, headers).then(() => {
      spinnerStop('Done!');
      spinnerStart('Creating relationships');
      createRelationships(tables, url, headers).then(() => {
        spinnerStop('Done!');
        const insertOrder = getInsertOrder(tables);
        insertData(insertOrder, db, tables, url, headers, async success => {
          if (level <= 10 && shouldNormalize) {
            normalize(tables, db, url, headers, level, importData);
          } else if (success) {
            spinnerStart('Dropping utility tables');
            const resp = await dropUtilityTables(url, headers);
            if (resp) {
              spinnerStop('Done!');
            }
            log('');
            log(`Success! Try out the GraphQL API at ${url}/console`, 'green');
            if (!shouldNormalize) {
              makeSuggestions();
            }
          }
        });
      });
    });
  });
};

module.exports = importData;
