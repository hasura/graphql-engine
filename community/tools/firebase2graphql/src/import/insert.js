const {query} = require('graphqurl');
const fetch = require('node-fetch');
const moment = require('moment');
const throwError = require('../error');
const {log, spinnerStart, spinnerStop, spinnerStopColorless} = require('../log');
const colors = require('colors/safe');

const getInsertOrder = tables => {
  let order = [];
  const tablesHash = {};
  tables.forEach(table => {
    tablesHash[table.name] = table;
  });
  const pushedHash = {};
  const setOrder = table => {
    if (table.dependencies.length === 0) {
      order.push(table.name);
      pushedHash[table.name] = true;
    } else {
      table.dependencies.forEach(parentTable => {
        if (!pushedHash[parentTable] && parentTable !== table.name) {
          setOrder(tablesHash[parentTable]);
        }
      });
      order.push(table.name);
      pushedHash[table.name] = true;
    }
  };

  tables.forEach(table => {
    if (!pushedHash[table.name]) {
      setOrder(table);
    }
  });
  return order;
};

const transformData = (data, tables) => {
  const newData = {};
  tables.forEach(table => {
    const tableData = data[table.name];
    newData[table.name] = [];
    tableData.forEach(row => {
      const newRow = {...row};
      table.columns.forEach(column => {
        if (column.type === 'timestamptz' && row[column.name]) {
          newRow[column.name] = moment(row[column.name]).format();
        }
        if (column.type === 'json' && row[column.name]) {
          newRow[column.name] = JSON.stringify(row[column.name]);
        }
      });
      newData[table.name].push(newRow);
    });
  });
  return newData;
};

const deleteDataTill = async (tableName, insertOrder, url, headers) => {
  spinnerStopColorless(colors.red('Error'));
  spinnerStart('Restoring database to a safe state');
  const truncate = async order => {
    const resp = await fetch(
      url,
      {
        method: 'POST',
        headers,
        body: JSON.stringify({
          type: 'run_sql',
          args: {
            sql: `truncate table public."${insertOrder[order]}" cascade;`,
            cascade: true,
          },
        }),
      }
    );
    if (insertOrder[order] === tableName) {
      spinnerStop('Done');
    } else {
      await truncate(order + 1, Boolean(resp));
    }
  };
  if (insertOrder.length === 0) {
    return;
  }
  return truncate(0);
};

const insertData = async (insertOrder, sampleData, tables, url, headers, callback) => {
  const transformedData = transformData(sampleData, tables);
  let numOfTables = insertOrder.length;
  const insertToTable = j => {
    if (j >= numOfTables) {
      callback(true);
      return true;
    }
    const tableName = insertOrder[j];
    const numOfRows = transformedData[tableName].length;
    let insertedRows = 0;
    const insertHundredRows = i => {
      let mutationString = '';
      let objectString = '';
      const variables = {};
      const numOfelementsToInsert = Math.min(numOfRows - insertedRows, 100);
      mutationString += `insert_${tableName} ( objects: $objects ) { affected_rows } \n`;
      objectString += `$objects: [${tableName}_insert_input!]!,\n`;
      variables.objects = [...transformedData[tableName].slice(i, numOfelementsToInsert + i)];
      const mutation = `mutation ( ${objectString} ) { ${mutationString} }`;
      spinnerStart(`Inserting ${i} to ${i + numOfelementsToInsert} rows of ${numOfRows} in table ${tableName}`);
      return query(
        {
          query: mutation,
          endpoint: `${url}/v1/graphql`,
          variables,
          headers,
        }
      ).then(response => {
        if (response.data) {
          spinnerStop('Done!');
          insertedRows += numOfelementsToInsert;
          if (insertedRows >= numOfRows) {
            return insertToTable(j + 1);
          }
          return insertHundredRows(i + 100);
        }
        deleteDataTill(tableName, insertOrder, url, headers).then(() => {
          throwError(
            JSON.stringify(response, null, 2),
            () => {
              log('Message: Schema has been imported. But the data could not be inserted due to the following error.', 'yellow');
              callback(false);
            }
          );
        });
      }).catch(e => {
        deleteDataTill(tableName, insertOrder, url, headers).then(() => {
          throwError(
            JSON.stringify(e, null, 2),
            () => {
              log('Message: Schema has been imported. But the data could not be imported due to the following error.', 'yellow');
              callback(false);
            }
          );
        });
      });
    };
    insertHundredRows(0);
  };
  return insertToTable(0);
};

module.exports = {
  getInsertOrder,
  insertData,
};
