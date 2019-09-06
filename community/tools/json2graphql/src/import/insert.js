const {query} = require('graphqurl');
const moment = require('moment');
const {cli} = require('cli-ux');
const throwError = require('./error');

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
        if (column.type === 'jsonb' && row[column.name]) {
          newRow[column.name] = JSON.stringify(row[column.name]);
        }
      });
      newData[table.name].push(newRow);
    });
  });
  return newData;
};

const insertData = async (insertOrder, sampleData, tables, url, headers) => {
  const transformedData = transformData(sampleData, tables);
  let mutationString = '';
  let objectString = '';
  const variables = {};
  insertOrder.forEach(tableName => {
    mutationString += `insert_${tableName} ( objects: $objects_${tableName} ) { affected_rows } \n`;
    objectString += `$objects_${tableName}: [${tableName}_insert_input!]!,\n`;
    variables[`objects_${tableName}`] = transformedData[tableName];
  });
  const mutation = `mutation ( ${objectString} ) { ${mutationString} }`;
  cli.action.start('Inserting data');
  try {
    const response = await query({
      query: mutation,
      endpoint: `${url}/v1/graphql`,
      variables,
      headers,
    });
    if (response.data !== null && response.data !== 'undefined') {
      cli.action.stop('Done!');
    } else {
      throw new Error(response);
    }
  } catch (e) {
    throwError(JSON.stringify(e, null, 2));
  }
};

module.exports = {
  getInsertOrder,
  insertData,
};
