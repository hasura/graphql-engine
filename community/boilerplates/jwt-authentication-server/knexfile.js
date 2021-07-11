// Update with your config settings.

const databaseName = "postgres";
const pg = require('pg');

const connection_url = process.env.DATABASE_URL || `postgres://postgres:@localhost:5432/${databaseName}`;

module.exports = {
  client: 'pg',
  connection: connection_url,
  migrations: {
    directory: __dirname + '/db/migrations'
  }
};
