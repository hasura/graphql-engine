import { makeDataAPIOptions, getColName } from '../../helpers/dataHelpers';
import { migrateModeUrl } from '../../helpers/common';
import { toggleOnMigrationMode } from '../data/migration-mode/utils';
// ***************** UTIL FUNCTIONS **************************

let accessKey;
let dataApiUrl;

export const setMetaData = () => {
  cy.window().then(win => {
    accessKey = win.__env.accessKey;
    dataApiUrl = win.__env.dataApiUrl;
    const { consoleMode } = win.__env;
    if (consoleMode === 'cli') {
      toggleOnMigrationMode();
    }
  });
};

export const createView = sql => {
  const reqBody = {
    type: 'run_sql',
    args: {
      sql,
    },
  };
  const requestOptions = makeDataAPIOptions(dataApiUrl, accessKey, reqBody);
  cy.request(requestOptions);
};

// ******************* VALIDATION FUNCTIONS *******************************

// ******************* Remote schema Validator ****************************
export const validateRS = (remoteSchemaName, result) => {
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
  const requestOptions = makeDataAPIOptions(dataApiUrl, accessKey, reqBody);
  cy.request(requestOptions).then(response => {
    if (result === 'success') {
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

// ****************** Table Validator *********************

export const validateCT = (tableName, result) => {
  const reqBody = {
    type: 'select',
    args: {
      table: tableName,
      columns: ['*'],
    },
  };
  const requestOptions = makeDataAPIOptions(dataApiUrl, accessKey, reqBody);
  cy.request(requestOptions).then(response => {
    if (result === 'success') {
      expect(response.status === 200).to.be.true;
    } else {
      expect(response.status === 200).to.be.false;
    }
  });
};

// **************** View Validator *******************

export const validateView = (viewName, result) => {
  validateCT(viewName, result);
};

// *************** Column Validator *******************

export const validateColumn = (tableName, column, result) => {
  const reqBody = {
    type: 'select',
    args: {
      table: tableName,
      columns: column,
    },
  };
  const requestOptions = makeDataAPIOptions(dataApiUrl, accessKey, reqBody);
  cy.request(requestOptions).then(response => {
    if (result === 'success') {
      expect(response.status === 200).to.be.true;
    } else {
      expect(response.status === 200).to.be.false;
    }
  });
};

export const validateColumnWhere = (tableName, column, where, result) => {
  const reqBody = {
    type: 'select',
    args: {
      table: tableName,
      columns: column,
      where,
    },
  };
  const requestOptions = makeDataAPIOptions(dataApiUrl, accessKey, reqBody);
  cy.request(requestOptions).then(response => {
    cy.log(JSON.stringify(response));
    if (result === 'success') {
      expect(response.body.length > 0).to.be.true;
    } else {
      expect(response.body.length === 0).to.be.true;
    }
  });
};

// ******************** Validate Insert *********************

export const validateInsert = (tableName, rows) => {
  const reqBody = {
    type: 'count',
    args: {
      table: tableName,
    },
  };
  const requestOptions = makeDataAPIOptions(dataApiUrl, accessKey, reqBody);
  cy.request(requestOptions).then(response => {
    cy.log(JSON.stringify(response));
    expect(response.body.count === rows).to.be.true;
  });
};

// ******************* Permissiosn Validator ****************

const compareChecks = (permObj, check, query, columns) => {
  if (check === 'none') {
    if (query === 'insert') {
      expect(Object.keys(permObj.check).length === 0).to.be.true;
    } else {
      expect(Object.keys(permObj.filter).length === 0).to.be.true;
      if (query === 'select' || query === 'update') {
        [0, 1, 2].forEach(index => {
          expect(permObj.columns.includes(getColName(index)));
        });
      }
    }
  } else if (query === 'insert') {
    // eslint-disable-line no-lonely-if
    expect(permObj.check[getColName(0)]._eq === '1').to.be.true; // eslint-dsable-line eqeqeq
  } else {
    expect(permObj.filter[getColName(0)]._eq === '1').to.be.true;
    if (query === 'select' || query === 'update') {
      columns.forEach((col, index) => {
        expect(permObj.columns.includes(getColName(index)));
      });
    }
  }
};

const handlePermValidationResponse = (
  tableSchema,
  role,
  query,
  check,
  result,
  columns
) => {
  const rolePerms = tableSchema.permissions.find(
    permission => permission.role_name === role
  );
  if (rolePerms) {
    const permObj = rolePerms.permissions[query];
    if (permObj) {
      compareChecks(permObj, check, query, columns);
    } else {
      // this block can be reached only if the permission doesn't exist (failure case)
      expect(result === 'failure').to.be.true;
    }
  } else {
    // this block can be reached only if the permission doesn't exist (failure case)
    expect(result === 'failure').to.be.true;
  }
};

export const validatePermission = (
  tableName,
  role,
  query,
  check,
  result,
  columns
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
  const requestOptions = makeDataAPIOptions(dataApiUrl, accessKey, reqBody);
  cy.request(requestOptions).then(response => {
    cy.log(JSON.stringify(response));
    const tableSchema = response.body.find(
      table => table.table_name === tableName
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

export const validateMigrationMode = mode => {
  cy.request({
    method: 'GET',
    url: migrateModeUrl,
  }).then(response => {
    expect(response.body.migration_mode == mode.toString()).to.be.true; // eslint-disable-line
  });
};

// ****************** Trigger Validator *********************

export const validateCTrigger = (triggerName, result) => {
  const reqBody = {
    type: 'select',
    args: {
      table: { name: 'event_triggers', schema: 'hdb_catalog' },
      columns: ['table_name'],
      where: { name: triggerName },
    },
  };
  const requestOptions = makeDataAPIOptions(dataApiUrl, accessKey, reqBody);
  cy.request(requestOptions).then(response => {
    if (result === 'success') {
      expect(response.status === 200).to.be.true;
    } else {
      expect(response.status === 200).to.be.false;
    }
  });
};
