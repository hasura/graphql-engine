const fetch = require('node-fetch');
const throwError = require('../error');
const {log, spinnerStart, spinnerStop} = require('../log');

const shouldIgnoreTable = table => {
  return (table.columns.find(c => c.name === '_value'));
};

const getDupeCandidates = tables => {
  const dupes = [];
  for (var i = tables.length - 1; i >= 0; i--) {
    const table = tables[i];
    if (shouldIgnoreTable(table)) {
      continue;
    }
    for (var j = tables.length - 1; j >= 0; j--) {
      if (table.name !== tables[j].name) {
        const dupeSuspect = tables[j];
        if (shouldIgnoreTable(dupeSuspect)) {
          continue;
        }
        let isDupe = true;
        for (var k = dupeSuspect.columns.length - 1; k >= 0; k--) {
          const columnName = dupeSuspect.columns[k].name;
          if (columnName.indexOf('_id') < 0) {
            if (!table.columns.find(col => col.name === columnName)) {
              isDupe = false;
            }
          }
        }
        if (isDupe) {
          dupes.push({
            table1: table.name,
            table2: dupeSuspect.name,
            columnList: dupeSuspect.columns.filter(dupeCol => dupeCol.name.indexOf('_id') < 0).map(dupeCol => dupeCol.name),
          });
        }
      }
    }
  }
  return dupes;
};

const categorizeDupeCandidates = async (dupes, url, headers) => {
  const bulkQueryArgs = [];
  dupes.forEach(dupe => {
    const {table1, table2, columnList} = dupe;
    const table1Sql = `select count(public."${table1}".*) from public."${table1}";`;
    const overlapSql = `select count(public."${table2}".*) from public."${table1}", public."${table2}"`;
    let whereSql = '';
    columnList.forEach((column, i) => {
      whereSql += ` public."${table1}"."${column}" = public."${table2}"."${column}"`;
      whereSql += i === columnList.length - 1 ? '' : ' and ';
    });
    const sql = `${overlapSql} where ${whereSql};`;
    bulkQueryArgs.push({
      type: 'run_sql',
      args: {
        sql: table1Sql,
      },
    });
    bulkQueryArgs.push({
      type: 'run_sql',
      args: {
        sql,
      },
    });
  });
  const response = await fetch(
    `${url}/v1/query`,
    {
      method: 'POST',
      headers,
      body: JSON.stringify({
        type: 'bulk',
        args: bulkQueryArgs,
      }),
    }
  );
  const respObj = await response.json();
  if (response.status !== 200) {
    throwError('Message: Could not normalize your data');
  }
  const newDupes = {
    confirmed: [],
    unconfirmed: [],
  };
  dupes.forEach((dupe, i) => {
    const overlapResult = respObj[(i * 2) + 1].result[1][0];
    const table1Count = respObj[i].result[1][0];
    if (!overlapResult || !table1Count) {
      throwError('Message: Could not normalize your data');
    }
    if (table1Count > 0 && overlapResult > 0) {
      if (table1Count === overlapResult) {
        newDupes.confirmed.push(dupe);
      } else if (overlapResult <= Number(table1Count) / 4) {
        newDupes.unconfirmed.push(dupe);
      } else {
        newDupes.confirmed.push(dupe);
      }
    }
  });
  return newDupes;
};

const patchDupeDependentTables = (table, dupe, tables, data, pkeyMap) => {
  const patchedData = {};
  tables.forEach(otherTable => {
    if (otherTable.name !== table && otherTable.name !== dupe) {
      if (otherTable.columns.find(column => {
        return column.name.indexOf(`${dupe}__id`) === 0 ||
          column.name.indexOf(`${table}__idself`) === 0;
      })) {
        const newData = data[otherTable.name].map(row => {
          const newRow = {
            ...row,
          };

          for (var c in row) {
            if (c.indexOf(`${table}__idself`) === 0) {
              delete newRow[c];
              continue;
            }
            if (c.indexOf(`${dupe}__idself`) === 0) {
              newRow[`${table}__id`] = pkeyMap[row[c]];
              delete newRow[c];
              continue;
            }
            if (c.indexOf(`${dupe}__id`) === 0) {
              delete newRow[c];
              continue;
            }
          }
          return newRow;
        });
        patchedData[otherTable.name] = newData;
      }
    }
  });
  return patchedData;
};

const makePkeyMap = (table, dupe, columnList, data) => {
  const map = {};
  data[dupe].forEach(dupeRow => {
    data[table].forEach(tableRow => {
      let isSameRow = true;
      columnList.forEach(column => {
        if (dupeRow[column] !== tableRow[column]) {
          isSameRow = false;
        }
      });
      if (isSameRow) {
        map[dupeRow._idself] = tableRow._id;
      }
    });
  });
  return map;
};

const getTablePriority = (table, dupe, topLevelTables) => {
  let isDupeTopLevel = false;
  let isTableTopLevel = false;
  for (var i = topLevelTables.length - 1; i >= 0; i--) {
    let row = topLevelTables[i];
    if (row.__tableName === dupe) {
      isDupeTopLevel = true;
    }
    if (row.__tableName === table) {
      isTableTopLevel = true;
    }
  }
  if (isDupeTopLevel && !isTableTopLevel) {
    return {
      table1: dupe,
      table2: table,
    };
  }
  if (!isDupeTopLevel && isTableTopLevel) {
    return {
      table1: table,
      table2: dupe,
    };
  }
  if (!isDupeTopLevel && !isTableTopLevel) {
    return {
      table1: table,
      table2: dupe,
    };
  }
  return {
    table1: null,
    table2: null,
  };
};

const handleConfirmedDupes = (confirmedDupes, tables, data) => {
  /*
  1. Go through the dupes
  2. Check which one of table1, table2 has _id (table) and _idself(dupe)
  3. Spread all fields of dupe in table
  4. Change column names and dependencies of all tables that have dupe as a dependency
  */
  let newData = {
    ...data,
  };
  let filteredTables = [...tables];
  const handle = (dupes, index) => {
    if (dupes.length === 0 || index > dupes.length - 1) {
      return;
    }
    const tableData = [];
    const {table1, table2} = getTablePriority(dupes[index].table1, dupes[index].table2, data.__rootTables);
    const columnList = dupes[index].columnList;
    if (!table1) {
      handle(dupes, index + 1);
      return;
    }
    const table = filteredTables.find(t => t.name === table1);
    const dupe = filteredTables.find(t => t.name === table2);
    newData[table.name].forEach(r => {
      const tableRow = {};
      for (var c in r) {
        if (c.indexOf('_idself') !== 0) {
          tableRow[c] = r[c];
        }
      }
      const dLength = data[dupe.name].length;
      let found = false;
      for (let j = 0; j < dLength; j++) {
        const dupeRow = newData[dupe.name][j];
        if (columnList.every(colName => dupeRow[colName] === tableRow[colName])) {
          found = true;
          const item = {};
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
      if (!found) {
        tableData.push(tableRow);
      }
    });
    newData[table.name] = tableData;
    filteredTables = filteredTables.filter(ft => ft.name !== dupe.name);
    newData = {
      ...newData,
      ...patchDupeDependentTables(table.name, dupe.name, filteredTables, newData, makePkeyMap(table1, table2, columnList, newData)),
    };
    delete newData[dupe.name];
    const filteredDupes = [];
    for (var i = dupes.length - 1; i >= 0; i--) {
      const d = dupes[i];
      if ((d.table1 !== table1 && d.table2 !== table2) && (d.table2 !== table1 && d.table1 !== table2)) {
        if (d.table1 === table2) {
          filteredDupes.push({
            table1,
            table2: d.table2,
          });
        }
        if (d.table2 === table2) {
          filteredDupes.push({
            table1,
            table2: d.table1,
          });
        }
      }
    }
    handle(
      filteredDupes,
      0
    );
  };
  handle(confirmedDupes, 0);
  return newData;
};

const dropTables = async (tableList, url, headers) => {
  spinnerStop('Done!');
  spinnerStart('Deleting unnecessary tables');
  if (tableList.length === 0) {
    spinnerStop('Done');
    return true;
  }
  let sql = '';
  tableList.forEach(t => {
    sql += `drop table if exists public."${t}" cascade;`;
  });
  const resp = await fetch(
    `${url}/v1/query`,
    {
      method: 'POST',
      headers,
      body: JSON.stringify({
        type: 'run_sql',
        args: {
          sql,
          cascade: true,
        },
      }),
    }
  );
  if (resp.status !== 200) {
    log('Message: Could not delete unnecessary tables. Your database might have some unnecessary tables.', 'yellow');
  }
  spinnerStop('Done');
  return true;
};

const normalize = async (tables, data, url, headers, level, importData) => {
  spinnerStart('Normalizing your data');
  const dupeCandidates = getDupeCandidates(tables);
  const maybeDupes = await categorizeDupeCandidates(dupeCandidates, url, headers);
  let newData;
  if (level === 10) {
    newData = handleConfirmedDupes(
      [...maybeDupes.confirmed, ...maybeDupes.unconfirmed],
      tables,
      data
    );
  } else {
    newData = handleConfirmedDupes(maybeDupes.confirmed, tables, data);
  }
  const tablesToDrop = tables.filter(t => newData[t.name] === undefined).map(tbl => tbl.name);
  const dropResp = await dropTables(tablesToDrop, url, headers);
  if (maybeDupes.unconfirmed.length === 0 && maybeDupes.confirmed.length === 0 && dropResp) {
    await importData(newData, url, headers, true, 11, true);
  } else {
    await importData(newData, url, headers, true, level + 1, true);
  }
};

module.exports = normalize;
