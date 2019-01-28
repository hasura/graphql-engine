const fetch = require('node-fetch');
const {cli} = require('cli-ux');
const throwError = require('./error');

const createTables = async (tables, url, headers, overwrite, runSql, sql) => {
  if (overwrite) {
    cli.action.stop('Skipped!');
    cli.action.start('Creating tables');
    await runSql(sql, url, headers);
  } else {
    try {
      const resp = await fetch(
        `${url}/v1/query`,
        {
          method: 'POST',
          headers,
          body: JSON.stringify({
            type: 'run_sql',
            args: {
              sql: "select * from information_schema.tables where table_schema = 'public';",
            },
          }),
        }
      );
      const dbTables = await resp.json();
      const tableIndex = dbTables.result[0].indexOf('table_name');
      let found = false;
      for (let i = dbTables.result.length - 1; i > 0; i--) {
        if (tables.find(t => t.name === dbTables.result[i][tableIndex])) {
          found = true;
          throwError('Message: Your JSON database contains tables that already exist in Postgres public schema. Please use the flag "--overwrite" to overwrite them.');
        }
      }
      if (!found) {
        cli.action.stop('Done!');
        cli.action.start('Creating tables');
        await runSql(sql, url, headers);
      }
    } catch (e) {
      console.log('Unexpected: ', e);
      process.exit(1);
    }
  }
};

module.exports = {
  createTables,
};
