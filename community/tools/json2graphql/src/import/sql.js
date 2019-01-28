const fetch = require('node-fetch');
const throwError = require('./error');

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
    table.columns.forEach((column, i) => {
      if (column.name === 'id') {
        columnSql += `"id" ${column.type} not null primary key`;
      } else {
        columnSql += `"${column.name}" ${column.type}`;
      }
      columnSql += (table.columns.length === i + 1) ? ' ) ' : ', ';
    });
    const createTableSql = `create table public."${table.name}" ${columnSql};`;
    sqlArray.push(createTableSql);
  });
  return sqlArray;
};

const generateConstraintsSql = metadata => {
  const sqlArray = [];
  metadata.forEach(table => {
    table.columns.forEach(column => {
      if (column.isForeign) {
        const fkSql = `add foreign key ("${column.name}") references public."${column.name.substring(0, column.name.length - 3)}" ("id");`;
        sqlArray.push(`alter table public."${table.name}" ${fkSql}`);
      }
    });
  });
  return sqlArray;
};

const generateSql = metadata => {
  const createTableSql = generateCreateTableSql(metadata);
  const constraintsSql = generateConstraintsSql(metadata);
  let sqlArray = [...createTableSql, ...constraintsSql];
  return sqlArray;
};

module.exports = {
  generateSql,
  runSql,
};
