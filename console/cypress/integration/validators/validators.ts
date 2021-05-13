import {
  makeDataAPIOptions,
  getColName,
  QueryEndpoint,
} from '../../helpers/dataHelpers';
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

export interface TableFields {
  [x: string]: any;
  id?: string;
  name?: string;
  title?: string;
  Content?: string;
  author_id?: string;
  rating?: string;
  user_id?: string;
  article_id?: string;
  comment?: string;
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
    type: 'export_metadata',
    args: {},
  };
  const requestOptions = makeDataAPIOptions(
    dataApiUrl,
    adminSecret,
    reqBody,
    'metadata'
  );
  cy.request(requestOptions).then(response => {
    const remoteSchemas = response.body?.remote_schemas ?? [];
    if (result === ResultType.SUCCESS) {
      expect(
        remoteSchemas.length > 0 && remoteSchemas[0].name === remoteSchemaName
      ).to.be.true;
    } else {
      expect(remoteSchemas.length > 0).to.be.false;
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
    type: 'export_metadata',
    args: {},
  };
  const requestOptions = makeDataAPIOptions(
    dataApiUrl,
    adminSecret,
    reqBody,
    'metadata'
  );
  cy.request(requestOptions).then(response => {
    const defaultSourceData = response.body.sources.find(
      (source: { name: string }) => source.name === 'default'
    );
    const functionData = defaultSourceData.functions;
    const foundFunction =
      functionData.filter(
        (fn: { function: { name: string; schema: string } }) =>
          fn.function.name === functionName &&
          fn.function.schema === functionSchema
      ).length === 1;
    if (result === ResultType.SUCCESS) {
      expect(functionData.length && foundFunction).to.be.true;
    } else {
      // NOTE: functionData.length may not be true
      expect(functionData.length && foundFunction).to.be.false;
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
    type: 'export_metadata',
    args: {},
  };
  const requestOptions = makeDataAPIOptions(
    dataApiUrl,
    adminSecret,
    reqBody,
    'metadata'
  );
  cy.request(requestOptions).then(response => {
    const defaultSourceData = response.body.sources.find(
      (source: { name: string }) => source.name === 'default'
    );
    const functionData = defaultSourceData?.functions ?? [];
    const foundFunction =
      functionData.filter(
        (fn: { function: { name: string; schema: string } }) =>
          fn?.function?.name === functionName &&
          fn?.function?.schema === functionSchema
      ).length === 1;
    if (result === ResultType.SUCCESS) {
      expect(!functionData.length || !foundFunction).to.be.true;
    } else {
      // NOTE: functionData.length may not be true
      expect(!functionData.length || !foundFunction).to.be.false;
    }
  });
};

/**
 * Make date API Request and check the repsone status
 * @param reqBody
 * @param result
 */
export const dataRequest = (
  reqBody: RequestBody,
  result: ResultType,
  queryType: QueryEndpoint = 'query'
) => {
  const requestOptions = makeDataAPIOptions(
    dataApiUrl,
    adminSecret,
    reqBody,
    queryType
  );
  cy.request(requestOptions).then(response => {
    if (result === ResultType.SUCCESS) {
      expect(
        (response.body.result_type === 'CommandOk' &&
          response.body.result === null) ||
          response.body.message === 'success'
      ).to.be.true;
    } else {
      expect(
        (response.body.result_type === 'CommandOk' &&
          response.body.result === null) ||
          response.body.message === 'success'
      ).to.be.false;
    }
  });
};

export const trackFunctionRequest = (
  reqBody: RequestBody,
  result: ResultType
) => {
  const requestOptions = makeDataAPIOptions(
    dataApiUrl,
    adminSecret,
    reqBody,
    'metadata'
  );
  cy.request(requestOptions).then(response => {
    if (result === ResultType.SUCCESS) {
      expect(response.body.message === ResultType.SUCCESS).to.be.true;
    } else {
      expect(response.body.message === ResultType.SUCCESS).to.be.false;
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
    type: 'run_sql',
    args: {
      sql: `SELECT * FROM "public"."${tableName}";`,
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
    source: 'default',
    args: {
      table: tableName,
      schema: 'public',
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
  const perm = permObj.permission ?? {};
  if (check === 'none') {
    if (query === 'insert') {
      expect(Object.keys(perm?.check ?? {}).length === 0).to.be.true;
      expect(perm?.set?.[getColName(0)] === '1').to.be.true;
      expect(perm?.set?.[getColName(1)] === 'x-hasura-user-id').to.be.true;
    } else {
      expect(Object.keys(perm?.filter ?? {}).length === 0).to.be.true;
      if (query === 'select' || query === 'update') {
        [0, 1, 2].forEach(index => {
          expect(perm?.columns.includes(getColName(index)));
        });
        if (query === 'update') {
          expect(perm?.set?.[getColName(0)] === '1').to.be.true;
          expect(perm?.set?.[getColName(1)] === 'x-hasura-user-id').to.be.true;
        }
      }
    }
  } else if (query === 'insert') {
    expect(perm?.check?.[getColName(0)]._eq === 1).to.be.true;
  } else {
    expect(perm?.filter?.[getColName(0)]._eq === 1).to.be.true;
    if (query === 'select' || query === 'update') {
      if (columns) {
        columns.forEach((col, index) => {
          expect(perm?.columns.includes(getColName(index)));
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
  let rolePerms = {};
  if (tableSchema?.[`${query}_permissions`]) {
    rolePerms = tableSchema[`${query}_permissions`].find(
      (permission: { role: string }) => permission.role === role
    );
  }

  if (Object.keys(rolePerms).length) {
    compareChecks(rolePerms, check, query, columns);
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
    type: 'export_metadata',
    args: {},
  };
  const requestOptions = makeDataAPIOptions(
    dataApiUrl,
    adminSecret,
    reqBody,
    'metadata'
  );
  cy.request(requestOptions).then(response => {
    const sourceInfo = response.body.sources.find(
      (source: { name: string }) => source.name === 'default'
    );
    const tableSchema = sourceInfo.tables.find(
      (table: { table: { name: string } }) => table.table.name === tableName
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
export const validateCTrigger = (
  triggerName: string,
  tableName: string,
  schemaName = 'public',
  result: ResultType
) => {
  const reqBody = {
    type: 'export_metadata',
    args: {},
  };
  const requestOptions = makeDataAPIOptions(
    dataApiUrl,
    adminSecret,
    reqBody,
    'metadata'
  );
  cy.request(requestOptions).then(response => {
    const sourceInfo = response.body.sources.find(
      (source: { name: string }) => source.name === 'default'
    );
    const tableInfo =
      sourceInfo?.tables?.find(
        (table: { table: { schema: string; name: string } }) =>
          table.table.schema === schemaName && table.table.name === tableName
      ) ?? {};
    const trigger =
      tableInfo?.event_triggers?.find(
        (trig: { name: string }) => trig.name === triggerName
      ) ?? {};

    if (result === ResultType.SUCCESS && Object.keys(tableInfo).length) {
      expect(response.status === 200).to.be.true;
      expect(trigger.definition.insert.columns === '*').to.be.true;
      expect(trigger.definition.delete.columns === '*').to.be.true;
      expect(trigger.definition.update.columns.length === 3).to.be.true;
      expect(
        trigger.retry_conf.interval_sec === parseInt(getIntervalSeconds(), 10)
      ).to.be.true;
      expect(trigger.retry_conf.num_retries === parseInt(getNoOfRetries(), 10))
        .to.be.true;
      expect(
        trigger.retry_conf.timeout_sec === parseInt(getTimeoutSeconds(), 10)
      ).to.be.true;
      expect(schemaName === 'public').to.be.true;
      expect(tableName === 'Apic_test_table_ctr_0').to.be.true;
    } else {
      expect(!Object.keys(tableInfo).length || !Object.keys(trigger).length).to
        .be.true;
    }
  });
};
