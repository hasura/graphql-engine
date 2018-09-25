const { spinnerStart, spinnerStop, log, successMessage } = require('../log');

const generate = require('./generateTables');
const { refineJson } = require('./utils');
const {generateSql, runSql} = require('./sql');
const colors = require('colors/safe');
const {trackTables} = require('./track');
const {getInsertOrder, insertData} = require('./insert');
const {createRelationships} = require('./relationships');
const {createTables} = require('./check');
const makeSuggestions = require('./suggest');
const generateGenericJson = require('../firebase/generateGenericJson');


const importData = async (jsonDb, url, headers, overwrite) => {
  spinnerStart('Processing Firebase JSON');
  const db = refineJson(generateGenericJson(jsonDb));
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
        spinnerStart('Inserting data');
        const insertOrder = getInsertOrder(tables);
        insertData(insertOrder, db, tables, url, headers).then(() => {
          spinnerStop('Done!');
          log('');
          log(`Success! Try out the GraphQL API at ${url}/console`, 'green');
          makeSuggestions(db, url);
        });
      });
    });
  });
};

module.exports = importData;
