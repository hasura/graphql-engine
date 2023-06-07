export const createTable = (tableName: string) => {
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
export const deleteTable = (tableName: string) => {
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

type DriverSpec = {
  name: 'postgres';
  helpers: {
    createTable: (tableName: string) => void;
    deleteTable: (tableName: string) => void;
  };
};

const postgres: DriverSpec = {
  name: 'postgres',
  helpers: {
    createTable,
    deleteTable,
  },
};

export { postgres };
