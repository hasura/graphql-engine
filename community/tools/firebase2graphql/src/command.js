const {Command, flags} = require('@oclif/command');
const fetch = require('node-fetch');
const {CLIError} = require('@oclif/errors');
const throwError = require('./error');
const {spinnerStart, spinnerStop} = require('./log');
const resolve = require('path').resolve;
const importData = require('./import/import');

class Firebase2GraphQL extends Command {
  async run() {
    const {args, flags} = this.parse(Firebase2GraphQL);
    const {url} = args;
    if (!url) {
      throw new CLIError('endpoint is required: \'firebase2graphql <url>\'');
    }
    const {db, overwrite, normalize} = flags;
    const key = flags['access-key'];
    const secret = flags['admin-secret'];

    if (secret !== undefined && key !== undefined) {
      throw new CLIError('cannot use both flags "access-key" and "admin-secret"', 'use "admin-secret" for versions greater than v1.0.0-alpha37 and "access-key" otherwise');
    }

    if (!url) {
      throw new CLIError('endpoint is required: \'firebase2graphql <url> -d ./db.js\'');
    }
    const safeUrl = this.getSafeUrl(url);
    if (!db) {
      throw new CLIError('path to firebase JSON database is required: \'firebase2graphql <url> -d ./db.js\'');
    }
    const dbJson = this.getDbJson(db);
    const headers = {
      [secret ? 'x-hasura-admin-secret' : 'x-hasura-access-key']: secret || key,
    };
    const urlVerification = await this.verifyUrl(safeUrl, headers);
    if (urlVerification.error) {
      throwError(`Message: ${urlVerification.message}`);
    } else {
      spinnerStop('Done!');
      await importData(dbJson, safeUrl, headers, overwrite, 1, normalize);
    }
  }

  getDbJson(db) {
    return require(resolve(db));
  }

  getSafeUrl(url) {
    const urlLength = url.length;
    return url[urlLength - 1] === '/' ? url.slice(0, -1) : url;
  }

  async verifyUrl(url, headers) {
    try {
      spinnerStart('Verifying URL');
      const resp = await fetch(
        `${url}/v1/query`,
        {
          method: 'POST',
          headers,
          body: JSON.stringify({
            type: 'run_sql',
            args: {
              sql: 'select * from hdb_catalog.hdb_version;',
            },
          }),
        }
      );
      return resp.status === 200 ? {error: false} : {error: true, message: 'invalid access-key or admin-secret'};
    } catch (e) {
      return  {error: true, message: 'invalid URL'};
    }
  }
}

Firebase2GraphQL.description = `firebase2graphql: Import JSON data to Hasura GraphQL Engine
# Examples:

# Import data from a Firebase JSON database to Hasura GraphQL Engine without admin secret
json2graphql https://hge.herokuapp.com --db=./path/to/db.json

# Import data from a Firebase JSON database to Hasura GraphQL Engine with admin secret
json2graphql https://hge.herokuapp.com --db=./path/to/db.json -s <admin-secret>

# Import data from a Firebase JSON database to Hasura GraphQL Engine while normalizing it
json2graphql https://hge.herokuapp.com --db=./path/to/db.json -n
`;

Firebase2GraphQL.usage = 'URL [-s SECRET]';

Firebase2GraphQL.flags = {
  // add --version flag to show CLI version
  version: flags.version(),

  // add --help flag to show CLI version
  help: flags.help({char: 'h'}),

  // Admin secret to Hasura GraphQL Engine
  'admin-secret': flags.string({
    char: 's',
    description: 'Admin secret to Hasura GraphQL Engine (X-Hasura-Admin-Secret). Use the flag --access-key if GraphQL Engine version is older than v1.0.0-alpha38',
  }),

  // Access key to Hasura GraphQL Engine
  'access-key': flags.string({
    char: 'k',
    description: 'Access key to Hasura GraphQL Engine (X-Hasura-Access-Key). Use the flag --admin-secret if GraphQL Engine version is greater than v1.0.0-alpha37',
  }),

  db: flags.string({
    char: 'd',
    description: 'Path to the .js files that exports a JSON database',
  }),

  normalize: flags.boolean({
    char: 'n',
    description: 'Normalize the data as it is imported to GraphQL Engine',
  }),

  overwrite: flags.boolean({
    char: 'o',
    description: 'Overwrite tables if they exist',
  }),
};

Firebase2GraphQL.args = [
  {
    name: 'url',
    description: 'URL where Hasura GraphQL Engine is running',
  },
];

module.exports = Firebase2GraphQL;
