const fetch = require('node-fetch');
const {CLIError} = require('@oclif/errors');
const {cli} = require('cli-ux');

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
            type: 'select',
            args: {
              table: {
                name: 'hdb_table',
                schema: 'hdb_catalog',
              },
              columns: ['*.*'],
              where: {
                table_schema: 'public',
              },
            },
          })
        }
      );
      const dbTables = await resp.json();
      let found = false;
      tables.forEach((table) => {
        if(dbTables.find((dbTable) => dbTable.table_name === table.name)) {
          found = true;
          cli.action.stop('Error');
          console.log('Message: Your JSON database contains tables that already exist in Postgres. Please use the flag "--overwrite" to overwrite them.');
          process.exit(1);
        }
      });
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
