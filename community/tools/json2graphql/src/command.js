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

    if (!url) {
      throw new CLIError('endpoint is required: \'json2graphql <url> -d ./db.js\'');
    }
    const safeUrl = this.getSafeUrl(url);
    if (!db) {
      throw new CLIError('path to sample database is required: \'json2graphql <url> -d ./db.js\'');
    }
    const dbJson = this.getDbJson(db);
    const headers = key ? {'x-hasura-access-key': key} : {};
    const urlVerification = await this.verifyUrl(safeUrl, headers);
    if (urlVerification.error) {
      cli.action.stop('Error')
      console.log('Message: ', urlVerification.message);
      process.exit
    } else {
      cli.action.stop('Done!');
      await importData(dbJson, safeUrl, headers, overwrite);
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
      cli.action.start('Verifying URL');
      const resp = await fetch(
        `${url}/v1/version`,
        {
          method: 'GET',
          headers
        }
      );
      return resp.status === 200 ? {error: false} : { error: true, message: 'invalid access key'};
    } catch (e) {
      return  { error: true, message: 'invalid URL'}
    }
  }
}

JSON2GraphQL.description = `JSON Data Import: Import JSON data to Hasura GraphQL Engine
# Examples:

# Import data from a JSON file to Hasura GraphQL Engine without access key
json2graphql https://hge.herokuapp.com --db=./path/to/db.js

# Make a query with CLI auto complete (this will show a gql prompt)
json2graphql https://hge.herokuapp.com --access-key='<access-key>' --db=./path/to/db.js

`;

JSON2GraphQL.usage = 'URL [-k KEY]';

JSON2GraphQL.flags = {
  // add --version flag to show CLI version
  version: flags.version(),

  // add --help flag to show CLI version
  help: flags.help({char: 'h'}),

  // Access key to Hasura GraphQL Engine
  'access-key': flags.string({
    char: 'k',
    description: 'Access key to Hasura GraphQL Engine (X-Hasura-Access-Key)',
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
