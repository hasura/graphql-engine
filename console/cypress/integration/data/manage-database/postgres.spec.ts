import { driverSpecType } from './common.spec';

// Use this config for running locally
// const config = {
//   host: 'postgres',
//   port: '5432',
//   dbName: 'postgres',
//   username: 'postgres',
//   password: 'postgrespassword',
// };

const config = {
  host: 'localhost',
  port: '5432',
  dbName: 'gql_test',
  username: 'gql_test',
  password: '',
};

const dbUrl = `postgres://${config.username}:${config.password}@${config.host}:${config.port}/${config.dbName}`;

const fillDetailsForDbUrlForm = (dbName: string) => {
  cy.getBySel('database-display-name').type(dbName);
  cy.getBySel('database-type').select('postgres');
  cy.getBySel('database-url').type(dbUrl);
  cy.getBySel('max-connections').type('50');
  cy.getBySel('idle-timeout').type('180');
  cy.getBySel('retries').type('1');
};

const fillDetailsForConnParamsForm = (dbName: string) => {
  cy.get("input[type='radio']").eq(2).click();
  cy.getBySel('database-display-name').type(dbName);
  cy.getBySel('database-type').select('postgres');
  cy.getBySel('host').type(config.host);
  cy.getBySel('port').type(config.port);
  cy.getBySel('username').type(config.username);
  if (config.password) {
    cy.getBySel('password').type(config.password);
  }
  cy.getBySel('database-name').type(config.dbName);
};

const fillDetailsForEnvVarForm = (dbName: string) => {
  cy.get("input[type='radio']").eq(0).click();
  cy.getBySel('database-display-name').type(dbName);
  cy.getBySel('database-type').select('postgres');
  cy.getBySel('database-url-env').type('HASURA_GRAPHQL_DATABASE_URL');
};

export const createDB = (dbName: string) => {
  const postBody = {
    type: 'pg_add_source',
    args: {
      name: dbName,
      configuration: {
        connection_info: {
          database_url: dbUrl,
        },
      },
    },
  };
  cy.request('POST', 'http://localhost:8080/v1/metadata', postBody).then(
    response => {
      expect(response.body).to.have.property('message', 'success'); // true
    }
  );
};

export const removeDB = (dbName: string) => {
  const postBody = { type: 'pg_drop_source', args: { name: dbName } };
  cy.request('POST', 'http://localhost:8080/v1/metadata', postBody).then(
    response => {
      expect(response.body).to.have.property('message', 'success'); // true
    }
  );
};

const postgres: driverSpecType = {
  name: 'postgres',
  tests: {
    fillDetailsForDbUrlForm,
    fillDetailsForConnParamsForm,
    fillDetailsForEnvVarForm,
  },
  helpers: {
    createDB,
    removeDB,
  },
};

export { postgres };
