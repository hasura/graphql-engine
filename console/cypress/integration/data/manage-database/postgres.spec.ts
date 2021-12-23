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

const createDB = (dbName: string) => {
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

const removeDB = (dbName: string) => {
  const postBody = { type: 'pg_drop_source', args: { name: dbName } };
  cy.request('POST', 'http://localhost:8080/v1/metadata', postBody).then(
    response => {
      expect(response.body).to.have.property('message', 'success'); // true
    }
  );
  cy.reload();
};

const createTable = (tableName: string) => {
  const postBody = {
    type: 'run_sql',
    args: {
      source: 'default',
      sql: `CREATE TABLE "public"."${tableName}" ("id" serial NOT NULL, "name" text NOT NULL, "countryCode" text DEFAULT 'IN', PRIMARY KEY ("id") );`,
      cascade: false,
      read_only: false,
    },
  };
  cy.request('POST', 'http://localhost:8080/v2/query', postBody).then(
    response => {
      expect(response.body).to.have.property('result_type', 'CommandOk'); // true
    }
  );
};

const trackTable = (tableName: string) => {
  const postBody = {
    type: 'pg_track_table',
    args: {
      table: {
        name: tableName,
        schema: 'public',
      },
      source: 'default',
    },
  };
  cy.request('POST', 'http://localhost:8080/v1/metadata', postBody).then(
    response => {
      expect(response.body).to.have.property('message', 'success'); // true
    }
  );
};

const untrackTable = (tableName: string) => {
  const postBody = {
    type: 'pg_untrack_table',
    args: {
      table: {
        schema: 'public',
        name: tableName,
      },
      source: 'default',
    },
  };
  cy.request('POST', 'http://localhost:8080/v1/metadata', postBody).then(
    response => {
      expect(response.body).to.have.property('message', 'success'); // true
    }
  );
};

const deleteTable = (tableName: string) => {
  const postBody = {
    type: 'run_sql',
    args: {
      source: 'default',
      sql: `DROP table "public"."${tableName}";`,
      cascade: false,
      read_only: false,
    },
  };
  cy.request('POST', 'http://localhost:8080/v2/query', postBody).then(
    response => {
      expect(response.body).to.have.property('result_type', 'CommandOk'); // true
    }
  );
};

const createRemoteSchema = (remoteSchemaName: string) => {
  const postBody = {
    type: 'add_remote_schema',
    args: {
      name: remoteSchemaName,
      definition: {
        timeout_seconds: 60,
        forward_client_headers: false,
        headers: [],
        url: 'https://countries.trevorblades.com/',
      },
      comment: '',
    },
  };
  cy.request('POST', 'http://localhost:8080/v1/metadata', postBody).then(
    response => {
      expect(response.body).to.have.property('message', 'success'); // true
    }
  );
};

const deleteRemoteSchema = (remoteSchemaName: string) => {
  const postBody = {
    type: 'remove_remote_schema',
    args: {
      name: remoteSchemaName,
    },
  };
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
    createTable,
    createRemoteSchema,
    deleteRemoteSchema,
    deleteTable,
    trackTable,
    untrackTable,
  },
};

export { postgres };
