import { makeDataAPIOptions, getColName } from '../../helpers/dataHelpers';
import { migrateModeUrl } from '../../helpers/common';
import { toggleOnMigrationMode } from '../data/migration-mode/utils';
import {
  getNoOfRetries,
  getIntervalSeconds,
  getTimeoutSeconds,
} from '../../helpers/eventHelpers';

// ***************** UTIL FUNCTIONS **************************

let adminSecret: string;
let dataApiUrl: string;

/* eslint-disable no-underscore-dangle */
export const setMetaData = () => {
  cy.window().then(win => {
    adminSecret = win.__env.adminSecret;
    dataApiUrl = win.__env.dataApiUrl;
    const { consoleMode } = win.__env;
    if (consoleMode === 'cli') {
      toggleOnMigrationMode();
    }
  });
};
/* eslint-enable no-underscore-dangle */

export const createView = (sql: string) => {
  const reqBody = {
    type: 'run_sql',
    args: {
      sql,
    },
  };
  const requestOptions = makeDataAPIOptions(dataApiUrl, adminSecret, reqBody);
  cy.request(requestOptions);
};

// ******************* VALIDATION FUNCTIONS *******************************

export enum ResultType {
  SUCCESS = 'success',
  FAILURE = 'failure',
}
interface RequestBody {
  [key: string]: any;
}
/**
 * Remote Schema validator
 * @param remoteSchemaName
 * @param result
 */
export const validateRS = (
  remoteSchemaName: string,
  result: ResultType
): void => {
  const reqBody = {
    type: 'select',
    args: {
      table: {
        name: 'remote_schemas',
        schema: 'hdb_catalog',
      },
      columns: ['*'],
      where: {
        name: remoteSchemaName,
      },
    },
  };
  const requestOptions = makeDataAPIOptions(dataApiUrl, adminSecret, reqBody);
  cy.request(requestOptions).then(response => {
    if (result === ResultType.SUCCESS) {
      expect(
        response.body.length > 0 && response.body[0].name === remoteSchemaName
      ).to.be.true;
    } else {
      expect(
        response.body.length > 0 && response.body[0].name === remoteSchemaName
      ).to.be.false;
    }
  });
};

/**
 * Vaidate the given function
 * @param functionName
 * @param functionSchema
 * @param result
 */
export const validateCFunc = (
  functionName: string,
  functionSchema: string,
  result: ResultType
): void => {
  const reqBody = {
    type: 'select',
    args: {
      table: {
        name: 'hdb_function',
        schema: 'hdb_catalog',
      },
      columns: ['*'],
      where: {
        function_name: functionName,
        function_schema: functionSchema,
      },
    },
  };
  const requestOptions = makeDataAPIOptions(dataApiUrl, adminSecret, reqBody);
  cy.request(requestOptions).then(response => {
    if (result === ResultType.SUCCESS) {
      expect(
        response.body.length > 0 &&
          response.body[0].function_name === functionName
      ).to.be.true;
    } else {
      expect(
        response.body.length > 0 &&
          response.body[0].function_name === functionName
      ).to.be.false;
    }
  });
};

/**
 * Validate untracked function
 * @param functionName
 * @param functionSchema
 * @param result
 */
export const validateUntrackedFunc = (
  functionName: string,
  functionSchema: string,
  result: ResultType
): void => {
  const reqBody = {
    type: 'select',
    args: {
      table: {
        name: 'hdb_function',
        schema: 'hdb_catalog',
      },
      columns: ['*'],
      where: {
        function_name: functionName,
        function_schema: functionSchema,
      },
    },
  };
  const requestOptions = makeDataAPIOptions(dataApiUrl, adminSecret, reqBody);
  cy.request(requestOptions).then(response => {
    if (result === ResultType.SUCCESS) {
      expect(response.body.length === 0).to.be.true;
    } else {
      expect(response.body.length === 0).to.be.false;
    }
  });
};

/**
 * Make date API Request and check the repsone status
 * @param reqBody
 * @param result
 */
export const dataRequest = (reqBody: RequestBody, result: ResultType) => {
  const requestOptions = makeDataAPIOptions(dataApiUrl, adminSecret, reqBody);
  cy.request(requestOptions).then(response => {
    if (result === ResultType.SUCCESS) {
      expect(
        response.body.length > 0 &&
          response.body[0].result_type === 'CommandOk' &&
          response.body[1].message === ResultType.SUCCESS
      ).to.be.true;
    } else {
      expect(
        response.body.length > 0 &&
          response.body[0].result_type === 'CommandOk' &&
          response.body[1].message === ResultType.SUCCESS
      ).to.be.false;
    }
  });
};

/**
 * Drop a table request
 * @param reqBody
 * @param result
 */
export const dropTableRequest = (reqBody: RequestBody, result: ResultType) => {
  const requestOptions = makeDataAPIOptions(dataApiUrl, adminSecret, reqBody);
  cy.request(requestOptions).then(response => {
    if (result === ResultType.SUCCESS) {
      expect(
        response.body.length > 0 && response.body[0].result_type === 'CommandOk'
      ).to.be.true;
    } else {
      expect(
        response.body.length > 0 && response.body[0].result_type === 'CommandOk'
      ).to.be.false;
    }
  });
};

// ****************** Table Validator *********************

export const validateCT = (tableName: string, result: ResultType) => {
  const reqBody = {
    type: 'select',
    args: {
      table: tableName,
      columns: ['*'],
    },
  };
  const requestOptions = makeDataAPIOptions(dataApiUrl, adminSecret, reqBody);
  cy.request(requestOptions).then(response => {
    if (result === ResultType.SUCCESS) {
      expect(response.status === 200).to.be.true;
    } else {
      expect(response.status === 200).to.be.false;
    }
  });
};

// **************** View Validator *******************

export const validateView = (viewName: string, result: ResultType) => {
  validateCT(viewName, result);
};

// *************** Column Validator *******************

export const validateColumn = (
  tableName: string,
  column: (string | { name: string; columns: string[] })[],
  result: ResultType
) => {
  const reqBody = {
    type: 'select',
    args: {
      table: tableName,
      columns: column,
    },
  };
  const requestOptions = makeDataAPIOptions(dataApiUrl, adminSecret, reqBody);
  cy.request(requestOptions).then(response => {
    if (result === ResultType.SUCCESS) {
      expect(response.status === 200).to.be.true;
    } else {
      expect(response.status === 200).to.be.false;
    }
  });
};

export const validateColumnWhere = (
  tableName: string,
  column: string,
  where: string,
  result: ResultType
) => {
  const reqBody = {
    type: 'select',
    args: {
      table: tableName,
      columns: column,
      where,
    },
  };
  const requestOptions = makeDataAPIOptions(dataApiUrl, adminSecret, reqBody);
  cy.request(requestOptions).then(response => {
    cy.log(JSON.stringify(response));
    if (result === ResultType.SUCCESS) {
      expect(response.body.length > 0).to.be.true;
    } else {
      expect(response.body.length === 0).to.be.true;
    }
  });
};

// ******************** Validate Insert *********************

export const validateInsert = (tableName: string, rows: number) => {
  const reqBody = {
    type: 'count',
    args: {
      table: tableName,
    },
  };
  const requestOptions = makeDataAPIOptions(dataApiUrl, adminSecret, reqBody);
  cy.request(requestOptions).then(response => {
    cy.log(JSON.stringify(response));
    expect(response.body.count === rows).to.be.true;
  });
};

// ******************* Permissiosn Validator ****************

export type QueryType = 'insert' | 'select' | 'update' | 'delete';
export type CheckType = 'custom' | 'none';
interface SchemaObject {
  [key: string]: any;
}

const compareChecks = (
  permObj: SchemaObject,
  check: CheckType,
  query: QueryType,
  columns: string[] | null
) => {
  if (check === 'none') {
    if (query === 'insert') {
      expect(Object.keys(permObj.check).length === 0).to.be.true;
      expect(permObj.set[getColName(0)] === '1').to.be.true;
      expect(permObj.set[getColName(1)] === 'x-hasura-user-id').to.be.true;
    } else {
      expect(Object.keys(permObj.filter).length === 0).to.be.true;
      if (query === 'select' || query === 'update') {
        [0, 1, 2].forEach(index => {
          expect(permObj.columns.includes(getColName(index)));
        });
        if (query === 'update') {
          expect(permObj.set[getColName(0)] === '1').to.be.true;
          expect(permObj.set[getColName(1)] === 'x-hasura-user-id').to.be.true;
        }
      }
    }
  } else if (query === 'insert') {
    // eslint-disable-next-line no-underscore-dangle
    expect(permObj.check[getColName(0)]._eq === 1).to.be.true;
  } else {
    // eslint-disable-next-line no-underscore-dangle
    expect(permObj.filter[getColName(0)]._eq === 1).to.be.true;
    if (query === 'select' || query === 'update') {
      if (columns) {
        columns.forEach((col, index) => {
          expect(permObj.columns.includes(getColName(index)));
        });
      }
    }
  }
};

const handlePermValidationResponse = (
  tableSchema: SchemaObject,
  role: string,
  query: QueryType,
  check: CheckType,
  result: ResultType,
  columns: string[] | null
) => {
  const rolePerms = tableSchema.permissions.find(
    (permission: { role_name: string }) => permission.role_name === role
  );
  if (rolePerms) {
    const permObj = rolePerms.permissions[query];
    if (permObj) {
      compareChecks(permObj, check, query, columns);
    } else {
      // this block can be reached only if the permission doesn't exist (failure case)
      expect(result === ResultType.FAILURE).to.be.true;
    }
  } else {
    // this block can be reached only if the permission doesn't exist (failure case)
    expect(result === ResultType.FAILURE).to.be.true;
  }
};

/**
 * Validate Permissions
 * @param tableName
 * @param role
 * @param query
 * @param check
 * @param result
 * @param columns
 */
export const validatePermission = (
  tableName: string,
  role: string,
  query: QueryType,
  check: CheckType,
  result: ResultType,
  columns: string[] | null
) => {
  const reqBody = {
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
  };
  const requestOptions = makeDataAPIOptions(dataApiUrl, adminSecret, reqBody);
  cy.request(requestOptions).then(response => {
    const tableSchema = response.body.find(
      (table: { table_name: string }) => table.table_name === tableName
    );
    handlePermValidationResponse(
      tableSchema,
      role,
      query,
      check,
      result,
      columns
    );
  });
};

// ********************** Validate Migration mode ******************
/**
 * Validate the migration mode
 * @param mode
 */
export const validateMigrationMode = (mode: boolean) => {
  cy.request({
    method: 'GET',
    url: migrateModeUrl,
  }).then(response => {
    expect(response.body.migration_mode == mode.toString()).to.be.true; // eslint-disable-line
  });
};

// ****************** Trigger Validator *********************
/**
 * Validate the trigger based on trigger name
 * @param triggerName
 * @param result
 */
export const validateCTrigger = (triggerName: string, result: ResultType) => {
  const reqBody = {
    type: 'select',
    args: {
      table: { name: 'event_triggers', schema: 'hdb_catalog' },
      columns: ['*'],
      where: { name: triggerName },
    },
  };
  const requestOptions = makeDataAPIOptions(dataApiUrl, adminSecret, reqBody);
  cy.request(requestOptions).then(response => {
    if (result === ResultType.SUCCESS) {
      expect(response.status === 200).to.be.true;
      expect(response.body.length === 1).to.be.true;
      const trigger = response.body[0];
      expect(trigger.configuration.definition.insert.columns === '*').to.be
        .true;
      expect(trigger.configuration.definition.delete.columns === '*').to.be
        .true;
      expect(trigger.configuration.definition.update.columns.length === 3).to.be
        .true;
      expect(
        trigger.configuration.retry_conf.interval_sec ===
          parseInt(getIntervalSeconds(), 10)
      ).to.be.true;
      expect(
        trigger.configuration.retry_conf.num_retries ===
          parseInt(getNoOfRetries(), 10)
      ).to.be.true;
      expect(
        trigger.configuration.retry_conf.timeout_sec ===
          parseInt(getTimeoutSeconds(), 10)
      ).to.be.true;
      expect(trigger.schema_name === 'public').to.be.true;
      expect(trigger.table_name === 'Apic_test_table_ctr_0').to.be.true;
    } else {
      expect(response.body.length === 0).to.be.true;
    }
  });
};
