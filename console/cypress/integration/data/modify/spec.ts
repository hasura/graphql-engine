import {
  tableColumnTypeSelector,
  baseUrl,
  getTableName,
  getColName,
  getElementFromAlias,
  createUntrackedFunctionSQL,
  dropUntrackedFunctionSQL,
  getIndexRoute,
} from '../../../helpers/dataHelpers';

import {
  setMetaData,
  validateCT,
  validateColumn,
  ResultType,
  dataRequest,
} from '../../validators/validators';
import { setPromptValue } from '../../../helpers/common';

const testName = 'mod';

export const passMTFunctionList = () => {
  const tableName = getTableName(0, testName);
  dataRequest(
    createUntrackedFunctionSQL(`${tableName}_id_fn`, tableName),
    ResultType.SUCCESS
  );
  cy.wait(5000);
  cy.get(getElementFromAlias('modify-table-edit-computed-field-0')).click();

  cy.get(getElementFromAlias('functions-dropdown')).click();

  cy.get('[data-test^="data_test_column_type_value_"]').should(
    'have.length',
    1
  );

  cy.get('[data-test^="data_test_column_type_value_"]')
    .first()
    .should('have.text', `${getTableName(0, testName)}_id_fn`.toLowerCase());
  dataRequest(
    dropUntrackedFunctionSQL(`${tableName}_id_fn`),
    ResultType.SUCCESS
  );
};

export const passMTCreateTable = () => {
  cy.get(getElementFromAlias('data-create-table')).click();
  cy.url().should('eq', `${baseUrl}/data/default/schema/public/table/add`);
  cy.get(getElementFromAlias('tableName')).type(getTableName(0, testName));
  cy.get(getElementFromAlias('column-0')).type('id');
  tableColumnTypeSelector('col-type-0');
  cy.get(getElementFromAlias('data_test_column_type_value_integer'))
    .first()
    .click();
  cy.get(getElementFromAlias('primary-key-select-0')).select('id');
  cy.get(getElementFromAlias('table-create')).click();
  cy.wait(7000);
  cy.url().should(
    'eq',
    `${baseUrl}/data/default/schema/public/tables/${getTableName(
      0,
      testName
    )}/modify`
  );
  validateCT(getTableName(0, testName), ResultType.SUCCESS);
};

export const passMTCheckRoute = () => {
  // Click on the create table button
  cy.get(getElementFromAlias('table-modify')).click();
  // Match the URL
  cy.url().should(
    'eq',
    `${baseUrl}/data/default/schema/public/tables/${getTableName(
      0,
      testName
    )}/modify`
  );
};

export const passMTRenameTable = () => {
  cy.get(getElementFromAlias('heading-edit-table')).click();
  cy.get(getElementFromAlias('heading-edit-table-input'))
    .clear()
    .type(getTableName(3, testName));
  cy.get(getElementFromAlias('heading-edit-table-save')).click();
  cy.wait(15000);
  validateCT(getTableName(3, testName), ResultType.SUCCESS);
  cy.get(getElementFromAlias('heading-edit-table')).click();
  cy.get(getElementFromAlias('heading-edit-table-input'))
    .clear()
    .type(getTableName(0, testName));
  cy.get(getElementFromAlias('heading-edit-table-save')).click();
  cy.wait(15000);
  validateCT(getTableName(0, testName), ResultType.SUCCESS);
};

export const passMTRenameColumn = () => {
  cy.wait(10000);
  cy.get(getElementFromAlias('modify-table-edit-column-0')).click();
  cy.get(getElementFromAlias('edit-col-name')).clear().type(getColName(3));
  cy.get(getElementFromAlias('modify-table-column-0-save')).click();
  cy.wait(15000);
  validateColumn(
    getTableName(0, testName),
    [getColName(3)],
    ResultType.SUCCESS
  );
  cy.get(getElementFromAlias('modify-table-edit-column-0')).click();
  cy.get(getElementFromAlias('edit-col-name')).clear().type('id');
  cy.get(getElementFromAlias('modify-table-column-0-save')).click();
  cy.wait(15000);
  validateColumn(getTableName(0, testName), ['id'], ResultType.SUCCESS);
};

export const passMTChangeDefaultValueForPKey = () => {
  cy.wait(10000);
  cy.get(getElementFromAlias('modify-table-edit-column-0')).click();
  cy.get(getElementFromAlias('edit-col-default')).clear().type('1234');
  cy.get(getElementFromAlias('modify-table-column-0-save')).click();
  cy.wait(15000);
};

export const passMTMoveToTable = () => {
  cy.get(getElementFromAlias(getTableName(0, testName))).click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/default/schema/public/tables/${getTableName(
      0,
      testName
    )}/browse`
  );
};

export const failMTWithoutColName = () => {
  cy.get(getElementFromAlias('modify-table-edit-add-new-column')).click();
  cy.get(getElementFromAlias('modify-table-add-new-column-save')).click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/default/schema/public/tables/${getTableName(
      0,
      testName
    )}/modify`
  );

  validateColumn(
    getTableName(0, testName),
    [getColName(2)],
    ResultType.FAILURE
  );
};

export const failMTWithoutColType = () => {
  cy.get(getElementFromAlias('column-name')).type(getColName(2));
  cy.get(getElementFromAlias('modify-table-add-new-column-save')).click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/default/schema/public/tables/${getTableName(
      0,
      testName
    )}/modify`
  );
  validateColumn(
    getTableName(0, testName),
    [getColName(2)],
    ResultType.FAILURE
  );
};

export const Addcolumnnullable = () => {
  cy.get(getElementFromAlias('column-name')).type('{selectall}{del}');
  cy.get(getElementFromAlias('column-name')).type(getColName(3));
  tableColumnTypeSelector('col-type-0');
  cy.get(getElementFromAlias('data_test_column_type_value_text'))
    .first()
    .click();
  cy.get(getElementFromAlias('nullable-checkbox')).uncheck({ force: true });
  cy.get(getElementFromAlias('modify-table-add-new-column-save')).click();
  cy.wait(2500);
  cy.url().should(
    'eq',
    `${baseUrl}/data/default/schema/public/tables/${getTableName(
      0,
      testName
    )}/modify`
  );
  validateColumn(
    getTableName(0, testName),
    [getColName(3)],
    ResultType.FAILURE
  );
};

export const Addcolumnname = (name: string) => {
  cy.get(getElementFromAlias('column-name')).type('{selectall}{del}');
  cy.get(getElementFromAlias('column-name')).type(name);

  tableColumnTypeSelector('col-type-0');
  cy.get(getElementFromAlias('data_test_column_type_value_integer'))
    .first()
    .click();

  cy.get(getElementFromAlias('modify-table-add-new-column-save')).click();
  cy.wait(5000);
  validateColumn(getTableName(0, testName), [name], ResultType.SUCCESS);
};

export const passMTAddColumn = () => {
  cy.get(getElementFromAlias('frequently-used-columns')).first().should('exist');
  cy.get(getElementFromAlias('column-name')).type('{selectall}{del}');
  cy.get(getElementFromAlias('column-name')).type(getColName(0));
  tableColumnTypeSelector('col-type-0');
  cy.get(getElementFromAlias('data_test_column_type_value_integer'))
    .first()
    .click();
  cy.get(getElementFromAlias('modify-table-add-new-column-save')).click();
  cy.wait(5000);
  validateColumn(
    getTableName(0, testName),
    [getColName(0)],
    ResultType.SUCCESS
  );
};

export const Movetocolumn = () => {
  Addcolumnname(getColName(1));
  cy.get(getElementFromAlias('modify-table-edit-column-1')).click();
};

export const failMCWithWrongDefaultValue = () => {
  cy.get(getElementFromAlias('modify-table-edit-column-1')).click();
  cy.get(getElementFromAlias('edit-col-default')).type('abcd');
  cy.get(getElementFromAlias('modify-table-column-1-save')).click();
};

export const passMCWithRightDefaultValue = () => {
  cy.get(getElementFromAlias('edit-col-default')).clear().type('1234');
  cy.get(getElementFromAlias('modify-table-column-1-save')).click();
  cy.wait(10000);
};

export const passCreateForeignKey = () => {
  cy.get(getElementFromAlias('modify-table-edit-fk-0')).click();
  cy.get(getElementFromAlias('foreign-key-ref-table-0')).select(
    getTableName(0, testName)
  );
  cy.get(getElementFromAlias('foreign-key-0-lcol-0')).select('0');
  cy.get(getElementFromAlias('foreign-key-0-rcol-0')).select('id');
  cy.get(getElementFromAlias('modify-table-fk-0-save')).click();
  cy.wait(10000);
};

export const passRemoveForeignKey = () => {
  cy.get(getElementFromAlias('modify-table-edit-fk-0')).click();
  cy.get(getElementFromAlias('modify-table-fk-0-remove')).click();
  cy.wait(10000);
};

export const passModifyPkey = () => {
  cy.get(getElementFromAlias('modify-table-edit-pks')).click();
  cy.get(getElementFromAlias('primary-key-select-1')).select('1');
  cy.get(getElementFromAlias('modify-table-pks-save')).click();
  cy.get(getElementFromAlias('pk-config-text')).within(() => {
    cy.get('b').contains(getColName(0));
    cy.get('b').contains('id');
  });
  cy.wait(5000);

  cy.get(getElementFromAlias('remove-pk-column-1')).click();
  cy.get(getElementFromAlias('modify-table-pks-save')).click();
  cy.get(getElementFromAlias('pk-config-text')).within(() => {
    cy.get('b').contains('id');
  });
  cy.get(getElementFromAlias('pk-config-text')).within(() => {
    cy.get('b').should('not.contain', getColName(0));
  });
  cy.get(getElementFromAlias('modify-table-close-pks')).click();
  cy.wait(3000);
};

export const passCreateUniqueKey = () => {
  cy.get(getElementFromAlias('modify-table-edit-unique-key-0')).click();
  cy.get(getElementFromAlias('unique-key-0-column-0')).select('0');
  cy.get(getElementFromAlias('unique-key-0-column-1')).select('1');
  cy.wait(1000);
  cy.get(getElementFromAlias('modify-table-unique-key-0-save')).click();
  cy.wait(5000);
  cy.get('div').contains(
    `${getTableName(0, testName)}_id_${getColName(0)}_key`
  );
};

export const passModifyUniqueKey = () => {
  cy.get(getElementFromAlias('modify-table-edit-unique-key-0')).click();
  cy.get(getElementFromAlias('remove-uk-0-column-0')).click();
  cy.get(getElementFromAlias('modify-table-unique-key-0-save')).click();
  cy.wait(5000);
  cy.get('div').contains(`${getTableName(0, testName)}_${getColName(0)}_key`);
};

export const passRemoveUniqueKey = () => {
  cy.get(getElementFromAlias('modify-table-edit-unique-key-0')).click();
  cy.get(getElementFromAlias('modify-table-unique-key-0-remove')).click();
  cy.wait(5000);
};

export const passMTDeleteCol = () => {
  setPromptValue(getColName(0));
  cy.get(getElementFromAlias('modify-table-edit-column-1')).click();
  cy.get(getElementFromAlias('modify-table-column-1-remove')).click();
  cy.window().its('prompt').should('be.called');
  cy.wait(5000);
  cy.url().should(
    'eq',
    `${baseUrl}/data/default/schema/public/tables/${getTableName(
      0,
      testName
    )}/modify`
  );
  validateColumn(
    getTableName(0, testName),
    [getColName(0)],
    ResultType.FAILURE
  );
};

export const passMTDeleteTableCancel = () => {
  setPromptValue(null);
  cy.get(getElementFromAlias('delete-table')).click();
  cy.window().its('prompt').should('be.called');
  cy.url().should(
    'eq',
    `${baseUrl}/data/default/schema/public/tables/${getTableName(
      0,
      testName
    )}/modify`
  );

  validateCT(getTableName(0, testName), ResultType.SUCCESS);
};

export const passMTDeleteTable = () => {
  setPromptValue(getTableName(0, testName));
  cy.get(getElementFromAlias('delete-table')).click();
  cy.window().its('prompt').should('be.called');
  cy.wait(5000);
  cy.url().should('eq', `${baseUrl}/data/default/schema/public`);
  validateCT(getTableName(0, testName), ResultType.FAILURE);
};

export const setValidationMetaData = () => {
  setMetaData();
};

//  Views Modify /////////////////////////////////////////////////

export const createTable = (name: string, dict: { [key: string]: any }) => {
  cy.url().should('eq', `${baseUrl}/data/schema/public/table/add`);
  cy.get(getElementFromAlias('tableName')).type(`${name}_table_mod`);
  const keys = Object.keys(dict).map(k => k);
  const values = Object.keys(dict).map(k => dict[k]);
  for (let i = 0; i < keys.length; i += 1) {
    cy.get('input[placeholder="column_name"]').last().type(keys[i]);
    cy.get('select')
      .find('option')
      .contains('-- type --')
      .parent()
      .last()
      .select(values[i]);
  }

  cy.get('select').last().select('id');
  cy.get(getElementFromAlias('table-create')).click();
  cy.wait(7000);
  cy.url().should(
    'eq',
    `${baseUrl}/data/default/schema/public/tables/${name}_table_mod/modify`
  );

  validateCT(`${name}_table_mod`, ResultType.SUCCESS);
};

export const Createtables = () => {
  cy.get(getElementFromAlias('data-create-table')).click();
  createTable('author', { id: 'integer', name: 'Text' });
  cy.visit(getIndexRoute());
  cy.wait(5000);
  cy.get(getElementFromAlias('data-create-table')).click();
  createTable('article', {
    id: 'integer',
    title: 'text',
    Content: 'text',
    author_id: 'integer',
    rating: 'integer',
  });
};

export const Createview = () => {
  cy.get(getElementFromAlias('sql-link')).click();
  cy.get('textarea').type(`CREATE VIEW author_average_rating_mod AS
    SELECT author_table_mod.id, avg(article_table.rating)
    From author_table_mod, article_table_mod
    WHERE author_table_mod.id = article_table_mod.author_id
    GROUP BY author_table_mod.id`);
  cy.get(getElementFromAlias('run-sql')).click();
  validateCT('author_average_rating_mod', ResultType.SUCCESS);
};

export const Checkviewtable = () => {
  cy.get(getElementFromAlias('author_average_rating_mod')).click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/default/schema/public/views/author_average_rating_mod/browse`
  );
  cy.get(getElementFromAlias('table-modify')).click();
  cy.get(getElementFromAlias('modify-view')).click();
  cy.url().should('eq', `${baseUrl}/data/sql`);
};

export const Checkviewtabledelete = () => {
  cy.get(getElementFromAlias('author_average_rating_mod')).click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/default/schema/public/views/author_average_rating_mod/browse`
  );
  cy.get(getElementFromAlias('table-modify')).click();
  setPromptValue('author_average_rating_mod');
  cy.get(getElementFromAlias('delete-view')).click();
  cy.window().its('prompt').should('be.called');

  cy.wait(7000);
  validateCT('author_average_rating_mod', ResultType.FAILURE);
};

export const Issue = () => {
  cy.get('.ace_text-input').first().type('#include');
};
