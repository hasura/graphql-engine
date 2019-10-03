const {Command, flags} = require('@oclif/command');
const fetch = require('node-fetch');
const {CLIError} = require('@oclif/errors');
const {cli} = require('cli-ux');
const importData = require('./import/import');
const resolve = require('path').resolve;

class JSON2GraphQL extends Command {
  async run() {
    const {args, flags} = this.parse(JSON2GraphQL);
    const {url} = args;
    if (!url) {
      throw new CLIError('endpoint is required: \'json2graphql <url>\'');
    }

    const {db, overwrite} = flags;
    const key = flags['access-key'];
    const secret = flags['admin-secret'];

    if (secret !== undefined && key !== undefined) {
      throw new CLIError('cannot use both flags "access-key" and "admin-secret"', 'use "admin-secret" for versions greater than v1.0.0-alpha37 and "access-key" otherwise');
    }

    if (!url) {
      throw new CLIError('endpoint is required: \'json2graphql <url> -d ./db.js\'');
    }
    const safeUrl = this.getSafeUrl(url);
    if (!db) {
      throw new CLIError('path to sample database is required: \'json2graphql <url> -d ./db.js\'');
    }
    const dbJson = await this.getDbJson(db);
    const headers = {
      [secret ? 'x-hasura-admin-secret' : 'x-hasura-access-key']: secret || key,
    };
    const urlVerification = await this.verifyUrl(safeUrl, headers);
    if (urlVerification.error) {
      throw new CLIError(urlVerification.message);
    } else {
      cli.action.stop('Done!');
      await importData(dbJson, safeUrl, headers, overwrite);
    }
  }

  getDbJson(db) {
    const ret = require(resolve(db));
    return typeof ret === 'function' ? ret() : ret;
  }

  getSafeUrl(url) {
    const urlLength = url.length;
    return url[urlLength - 1] === '/' ? url.slice(0, -1) : url;
  }

  async verifyUrl(url, headers) {
    try {
      cli.action.start('Verifying URL');
      const resp = await fetch(
        `${url}/v1/version`,
        {
          method: 'GET',
          headers,
        }
      );
      return resp.status === 200 ? {error: false} : {error: true, message: 'invalid admin-secret or access-key'};
    } catch (e) {
      return  {error: true, message: 'invalid URL'};
    }
  }
}

JSON2GraphQL.description = `JSON Data Import: Import JSON data to Hasura GraphQL Engine
# Examples:

# Import data from a JSON file to Hasura GraphQL Engine without admin secret
json2graphql https://hge.herokuapp.com --db=./path/to/db.js

# Import data from a JSON file to Hasura GraphQL Engine with admin secret
json2graphql https://hge.herokuapp.com --admin-secret='<admin-secret>' --db=./path/to/db.js

`;

JSON2GraphQL.usage = 'URL [-k KEY]';

JSON2GraphQL.flags = {
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

  overwrite: flags.boolean({
    char: 'o',
    description: 'Overwrite tables if they exist',
  }),
};

JSON2GraphQL.args = [
  {
    name: 'url',
    description: 'URL where Hasura GraphQL Engine is running',
  },
];

module.exports = JSON2GraphQL;
