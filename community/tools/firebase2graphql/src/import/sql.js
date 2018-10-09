const fetch = require('node-fetch');
const throwError = require('../error');

const runSql = async (sqlArray, url, headers) => {
  let sqlString = '';
  sqlArray.forEach(sql => {
    sqlString += sql;
  });
  const resp = await fetch(
    `${url}/v1/query`,
    {
      method: 'POST',
      body: JSON.stringify({
        type: 'run_sql',
        args: {
          sql: sqlString,
          cascade: true,
        },
      }),
      headers,
    }
  );
  if (resp.status !== 200) {
    const error = await resp.json();
    throwError(JSON.stringify(error, null, 2));
  }
};

const generateCreateTableSql = metadata => {
  const sqlArray = [];
  metadata.forEach(table => {
    sqlArray.push(`drop table if exists public."${table.name}" cascade;`);
    let columnSql = '(';
    const pkeyArr = [];
    table.columns.forEach((column, i) => {
      if (column.name.indexOf('_id') === 0) {
        pkeyArr.push(column.name);
        columnSql += `"${column.name}" ${column.type} not null,`;
      } else {
        columnSql += `"${column.name}" ${column.type},`;
      }

      if (table.columns.length === i + 1) {
        columnSql += 'primary key (';
        pkeyArr.forEach((key, j) => {
          columnSql += `"${key}"`;
          columnSql += j === pkeyArr.length - 1 ? ')' : ', ';
        });
      }
    });
    const createTableSql = `create table public."${table.name}" ${columnSql});`;
    sqlArray.push(createTableSql);
  });
  return sqlArray;
};

const foreignKeySql = table => {
  const sqlArray = [];
  table.dependencies.forEach((dep, i) => {
    let colNames = '';
    let fks = '';
    table.columns.forEach(col => {
      if (col.name.indexOf(`${dep}__id`) === 0) {
        colNames += `"${col.name}", `;
        fks += `"${col.name.substring(col.name.indexOf('_id'), col.name.length)}", `;
      }
    });
    fks = fks.substring(0, fks.length - 2);
    colNames = colNames.substring(0, colNames.length - 2);
    sqlArray.push(`alter table "${table.name}" add constraint "fk_${table.name}_${dep}_${i}" foreign key (${colNames}) references "${dep}"(${fks});`);
  });
  return sqlArray;
};

const generateConstraintsSql = metadata => {
  let sqlArray = [];
  metadata.forEach(table => {
    sqlArray = [
      ...sqlArray,
      ...foreignKeySql(table),
    ];
  });
  return sqlArray;
};

const generateSql = metadata => {
  const createTableSql = generateCreateTableSql(metadata);
  const constraintsSql = generateConstraintsSql(metadata);
  let sqlArray = [...createTableSql, ...constraintsSql];
  return sqlArray;
};

const dropUtilityTables = async (url, headers) => {
  const tablesToDrop = ['__rootTables'];
  let sql = '';
  tablesToDrop.forEach(table => {
    sql += `drop table if exists "${table}" cascade;`;
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
  return Boolean(resp);
};

module.exports = {
  generateSql,
  runSql,
  dropUtilityTables,
};
