import { driverSpecType } from './common.spec';

//  Use this config for running locally
// const config = {
//   host: '172.17.0.1',
//   port: '1433',
//   dbName: 'master',
//   username: 'SA',
//   password: 'reallyStrongPwd123',
// };

const config = {
  host: 'localhost',
  port: '1433',
  dbName: 'master',
  username: 'SA',
  password: 'hasuraMSSQL1',
};

const dbUrl = `DRIVER={ODBC Driver 17 for SQL Server};SERVER=${config.host};DATABASE=${config.dbName};Uid=${config.username};Pwd=${config.password}`;

const fillDetailsForDbUrlForm = (dbName: string) => {
  cy.getBySel('database-display-name').type(dbName);
  cy.getBySel('database-type').select('mssql');
  cy.getBySel('database-url').type(dbUrl, { parseSpecialCharSequences: false });
  cy.getBySel('max-connections').type('10');
  cy.getBySel('idle-timeout').type('100');
};

const fillDetailsForEnvVarForm = (dbName: string) => {
  cy.get("input[type='radio']").eq(0).click();
  cy.getBySel('database-display-name').type(dbName);
  cy.getBySel('database-type').select('mssql');
  cy.getBySel('database-url-env').type('TEST_MSSQL_DATABASE_URL');
};

export const removeDB = (dbName: string) => {
  const postBody = { type: 'mssql_drop_source', args: { name: dbName } };
  cy.request('POST', 'http://localhost:8080/v1/metadata', postBody).then(
    response => {
      expect(response.body).to.have.property('message', 'success'); // true
    }
  );
};

export const createDB = (dbName: string) => {
  const postBody = {
    type: 'mssql_add_source',
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

const mssql: driverSpecType = {
  name: 'mssql',
  tests: {
    fillDetailsForDbUrlForm,
    fillDetailsForEnvVarForm,
  },
  helpers: {
    removeDB,
    createDB,
  },
};

export { mssql };
