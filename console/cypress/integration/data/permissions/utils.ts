import {
  getElementFromAlias,
  getTableName,
  getColName,
  queryTypes,
  makeDataAPIOptions,
} from '../../../helpers/dataHelpers';

import {
  validatePermission,
  QueryType,
  ResultType,
  CheckType,
} from '../../validators/validators';

const testName = 'perm';

export const savePermission = () => {
  cy.get(getElementFromAlias('Save-Permissions-button')).click();
  cy.wait(7000);
};

export const permNoCheck = (tableName: string, query: QueryType) => {
  // click on the query type to edit permission
  cy.get(getElementFromAlias(`role0-${query}`)).click();
  cy.get(getElementFromAlias('without-checks'))
    .first()
    .click();
  // set filter { }
  // Toggle all columns in case
  if (query === 'select' || query === 'update') {
    cy.get(getElementFromAlias('toggle-all-col-btn')).click();
  }
  if (query === 'insert' || query === 'update') {
    cy.get(getElementFromAlias('toggle-presets-permission')).click();
    cy.get(getElementFromAlias('column-presets-column-0')).select(
      getColName(0)
    );
    cy.get(getElementFromAlias('column-presets-type-0')).select('static');
    cy.get(getElementFromAlias('column-presets-value-0'))
      .type('1')
      .blur();
    cy.get(getElementFromAlias('column-presets-column-1')).select(
      getColName(1)
    );
    cy.get(getElementFromAlias('column-presets-type-1')).select('session');
    cy.get(getElementFromAlias('column-presets-value-1')).type('user-id');
  }
  // Save
  savePermission();
  // Validate
  validatePermission(
    tableName,
    'role0',
    query,
    'none',
    ResultType.SUCCESS,
    null
  );
};

export const permCustomCheck = (tableName: string, query: QueryType) => {
  // click on the query type to edit permission
  cy.get(getElementFromAlias(`role0-${query}`)).click();
  // check the without checks textbox
  cy.get(getElementFromAlias('toggle-row-permission')).click();
  cy.get(getElementFromAlias('custom-check'))
    .first()
    .click();

  cy.get(getElementFromAlias('qb_container'))
    .first()
    .within(() => {
      // Select column
      cy.get(getElementFromAlias('qb-select'))
        .first()
        .select(getColName(0));
      // Select operator
      cy.get(getElementFromAlias('qb-select'))
        .last()
        .select(`${getColName(0)}._eq`);
    });
  // Set filter to 1
  cy.get(getElementFromAlias('perm-check-textbox'))
    .first()
    .type('1');
  // Save
  savePermission();
  // Validate
  validatePermission(
    tableName,
    'role0',
    query,
    'custom',
    ResultType.SUCCESS,
    [0, 1, 2].map(i => getColName(i))
  );
  // Do not allow users to make upset queries in case of Insert
};

export const permRemove = (tableName: string, query: QueryType) => {
  // click on the query type to edit permission
  cy.get(getElementFromAlias(`role0-${query}`)).click();
  // Remove permission
  cy.get(getElementFromAlias('Delete-Permissions-button')).click();
  cy.wait(2500);
  cy.wait(5000);
  // Validate
  validatePermission(
    tableName,
    'role0',
    query,
    'custom',
    ResultType.FAILURE,
    null
  );
};

export const testPermissions = (
  tableName: string,
  check: CheckType,
  isView?: boolean
) => {
  let allQueryTypes: QueryType[] = queryTypes;
  if (isView) {
    allQueryTypes = ['select'];
  }

  if (check === 'none') {
    allQueryTypes.forEach(query => {
      permNoCheck(tableName, query);
    });
  } else {
    allQueryTypes.forEach(query => {
      permCustomCheck(tableName, query);
    });
  }
};

export const trackView = () => {
  // track view
  cy.get('a')
    .contains('Data')
    .click();
  cy.wait(7000);
  cy.get(
    getElementFromAlias(`add-track-table-${getTableName(1, testName)}`)
  ).click();
  cy.wait(10000);
  // Move to permissions
  cy.get(getElementFromAlias('table-permissions')).click();
};

export const createView = (viewName: string, tableName: string) => {
  const reqBody = {
    type: 'run_sql',
    args: {
      sql: `create view "${viewName}" as select * from "${tableName}"`,
    },
  };
  cy.window().then(win => {
    const { __env } = win;
    const requestOptions = makeDataAPIOptions(
      __env.dataApiUrl,
      __env.adminSecret,
      reqBody
    );
    cy.request(requestOptions);
  });
};
