const fetch = require('node-fetch');
const throwError = require('../error');

const shouldIgnoreTable = (table) => {
  return (table.columns.find((c) => c.name === '__value'));
}

const getDupeCandidates = (tables, stage) => {
  const dupes = [];
  for (var i = tables.length - 1; i >= 0; i--) {
    const table = tables[i];
    if (shouldIgnoreTable(table)) { continue; }
    for (var j = tables.length - 1; j >=0; j--) {
      if (table.name !== tables[j].name) {
        const dupeSuspect = tables[j];
        if (shouldIgnoreTable(dupeSuspect)) { continue; }
        let isDupe = true;
        for (var k = dupeSuspect.columns.length - 1; k >=0; k--) {
          const columnName = dupeSuspect.columns[k].name;
          if (columnName.indexOf('_id') < 0) {
            if(!table.columns.find((col) => col.name === columnName)) {
              isDupe = false;
            }
          }
        }
        if (isDupe) {
          dupes.push({
            table1: table.name,
            table2: dupeSuspect.name,
            columnList: dupeSuspect.columns.filter((dupeCol) => dupeCol.name.indexOf('_id') < 0).map((dupeCol) => dupeCol.name)
          });
        }
      }
    }
  }
  return dupes;
}

const categorizeDupeCandidates = async (dupes, url, headers) => {
  const bulkQueryArgs = [];
  dupes.forEach((dupe) => {
    const { table1, table2, columnList } = dupe;
    const table1Sql = `select count(public."${table1}".*) from public."${table1}";`;
    const overlapSql = `select count(public."${table2}".*) from public."${table1}", public."${table2}"`;
    let whereSql = '';
    columnList.forEach((column, i) => {
      whereSql += ` public."${table1}"."${column}" = public."${table2}"."${column}"`;
      whereSql += i === columnList.length - 1 ? '' : ' and '
    })
    const sql = `${overlapSql} where ${whereSql};`;
    bulkQueryArgs.push({
      type: 'run_sql',
      args: {
        sql: table1Sql
      }
    });
    bulkQueryArgs.push({
      type: 'run_sql',
      args: {
        sql
      }
    });
  });
  const response = await fetch(
    `${url}/v1/query`,
    {
      method: 'POST',
      headers,
      body: JSON.stringify({
        type: 'bulk',
        args: bulkQueryArgs
      })
    }
  );
  const respObj = await response.json();
  console.log(JSON.stringify(respObj, null, 2));
  if (response.status !== 200) {
    throwError('Message: Could not normalize your data');
  }
  const newDupes = {
    confirmed: [],
    unconfirmed: []
  };
  dupes.forEach((dupe, i) => {
    const overlapResult = respObj[i*2 + 1].result[1][0];
    const table1Count = respObj[i].result[1][0];
    if (!overlapResult || !table1Count) {
      throwError('Message: Could not normalize your data');  
    }
    if (table1Count > 0 && overlapResult > 0) {
      if (table1Count === overlapResult) {
        newDupes.confirmed.push(dupe);
      } else {
        if (overlapResult <= table1Count * 1 / 4) {
          newDupes.unconfirmed.push(dupe);   
        } else {
          newDupes.confirmed.push(dupe);
        }
      }
    }
  });
  return newDupes;
};

const patchDupeDependentTables = (table, dupe, tables, data) => {
  const patchedData = {};
  tables.forEach((otherTable) => {
    if (otherTable.name !== table && otherTable.name !== dupe) {
      if (otherTable.columns.find((column) => column.name === `${dupe}_idself`)) {
        const newData = data[otherTable.name].map((row) => {
          const newRow = {
            ...row,
          };
          newRow[`${table}_id`] = row[`${dupe}_idself`];
          delete newRow[`${dupe}_idself`];
          return newRow;
        });
        patchedData[otherTable.name] = newData;
      }
    }
  });
  return patchedData;
};

const handleConfirmedDupes = (confirmedDupes, tables, data) => {
  /*
  1. Go through the dupes
  2. Check which one of table1, table2 has _id (table) and _idself(dupe)
  3. Spread all fields of dupe in table
  4. Change column names and dependencies of all tables that have dupe as a dependency
  */
  let newData = {
    ...data
  };
  const handle = (dupes, index) => {
    if (dupes.length === 0 || index > dupes.length - 1) {
      return;
    }
    const tableData = [];
    let table1, table2;
    const columnList = dupes[index].columnList;
    if (!newData[dupes[index].table1][0]['_idself'] && newData[dupes[index].table1][0]['_id']) {
      table1 = dupes[index].table1;
      table2 = dupes[index].table2;
    } else if (!newData[dupes[index].table2][0]['_idself'] && newData[dupes[index].table2][0]['_id']) {
      table2 = dupes[index].table1;
      table1 = dupes[index].table2;
    } else {
      handle(dupes, index + 1);
      return;
    }
    console.log(table1);
    console.log(table2);
    const table = tables.find((t) => t.name === table1);
    const dupe = tables.find((t) => t.name === table2);
    newData[table.name].forEach((tableRow) => {
      const dLength = data[dupe.name].length;
      for (let j = 0; j < dLength; j++) {
        const dupeRow = newData[dupe.name][j];
        if (columnList.every((colName) => dupeRow[colName] === tableRow[colName])) {
          const item = {}
          for (var key in dupeRow) {
            if (key.indexOf('_idself') !== 0) {
              item[key.replace(dupe.name + '_', table.name + '_')] = dupeRow[key];
            }
          }
          tableData.push({
            ...item,
            ...tableRow,
          });
          break;
        }
      }
    });
    newData[table.name] = tableData;
    delete newData[dupe.name];
    newData = {
      ...newData,
      ...patchDupeDependentTables(table.name, dupe.name, tables, data)
    }
    handle(
      dupes.filter((d) => d.table1 !== table1 && d.table2 !== table1 && d.table1 !== table2 && d.table2 !== table2),
      0
    )
  }
  handle(confirmedDupes, 0);
  return newData;
};

const normalize = async (tables, data, url, headers, level, importData) => {
  const dupeCandidates = getDupeCandidates(tables);
  const maybeDupes = await categorizeDupeCandidates(dupeCandidates, url, headers);
  const newData = handleConfirmedDupes(maybeDupes.confirmed, tables, data);
  if (maybeDupes.unconfirmed.length === 0) {
    await importData(newData, url, headers, true, 4);
  } else {
    await importData(newData, url, headers, true, level + 1);
  }
};

module.exports = normalize;