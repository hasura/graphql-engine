const generate = require('./generateTables');
const { refineJson } = require('./utils');
const {generateSql, runSql} = require('./sql');
const {cli} = require('cli-ux');
const {trackTables} = require('./track');
const {getInsertOrder, insertData} = require('./insert');
const {createRelationships} = require('./relationships');
const {createTables} = require('./check');

const importData = async (jsonDb, url, headers, overwrite, isFirebase) => {
  cli.action.start('Processing JSON data');
  const db = refineJson(jsonDb);
  const tables = generate(db, isFirebase);
  const sql = generateSql(tables, isFirebase);
  cli.action.stop('Done!');
  cli.action.start('Checking database');
  createTables(tables, url, headers, overwrite, runSql, sql).then(() => {
    cli.action.stop('Done!');
    cli.action.start('Tracking tables');
    trackTables(tables, url, headers).then(() => {
      cli.action.stop('Done!');
      cli.action.start('Creating relationships');
      createRelationships(tables, url, headers).then(() => {
        cli.action.stop('Done!');
        cli.action.start('Inserting data');
        const insertOrder = getInsertOrder(tables);
        insertData(insertOrder, db, tables, url, headers, isFirebase);
      });
    });
  });
};

module.exports = importData;
