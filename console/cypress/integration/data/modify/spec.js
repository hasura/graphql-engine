import {
  tableColumnTypeSelector,
  baseUrl,
  getTableName,
  getColName,
  getElementFromAlias,
} from '../../../helpers/dataHelpers';

import {
  setMetaData,
  validateCT,
  validateColumn,
} from '../../validators/validators';

const testName = 'mod';

export const passMTCreateTable = () => {
  cy.get(getElementFromAlias('data-create-table')).click();
  cy.url().should('eq', `${baseUrl}/data/schema/public/table/add`);
  cy.get(getElementFromAlias('tableName')).type(getTableName(0, testName));
  cy.get(getElementFromAlias('column-0')).type('id');
  tableColumnTypeSelector('col-type-0');
  cy.get(getElementFromAlias('data_test_column_type_value_integer'))
    .first()
    .click();
  // cy.get(getElementFromAlias('col-type-0')).select('Integer');
  cy.get(getElementFromAlias('primary-key-select-0')).select('id');
  cy.get(getElementFromAlias('table-create')).click();
  cy.wait(7000);
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0, testName)}/modify`
  );
  validateCT(getTableName(0, testName), 'success');
};

export const passMTCheckRoute = () => {
  // Click on the create table button
  cy.get(getElementFromAlias('table-modify')).click();
  // Match the URL
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0, testName)}/modify`
  );
};

export const passMTRenameTable = () => {
  cy.get(getElementFromAlias('heading-edit-table')).click();
  cy.get(getElementFromAlias('heading-edit-table-input'))
    .clear()
    .type(getTableName(3, testName));
  cy.get(getElementFromAlias('heading-edit-table-save')).click();
  cy.wait(15000);
  validateCT(getTableName(3, testName), 'success');
  cy.get(getElementFromAlias('heading-edit-table')).click();
  cy.get(getElementFromAlias('heading-edit-table-input'))
    .clear()
    .type(getTableName(0, testName));
  cy.get(getElementFromAlias('heading-edit-table-save')).click();
  cy.wait(15000);
  validateCT(getTableName(0, testName), 'success');
};

export const passMTRenameColumn = () => {
  cy.wait(10000);
  cy.get(getElementFromAlias('modify-table-edit-column-0')).click();
  cy.get(getElementFromAlias('edit-col-name'))
    .clear()
    .type(getColName(3));
  cy.get(getElementFromAlias('modify-table-column-0-save')).click();
  cy.wait(15000);
  validateColumn(getTableName(0, testName), [getColName(3)], 'success');
  cy.get(getElementFromAlias('modify-table-edit-column-0')).click();
  cy.get(getElementFromAlias('edit-col-name'))
    .clear()
    .type('id');
  cy.get(getElementFromAlias('modify-table-column-0-save')).click();
  cy.wait(15000);
  validateColumn(getTableName(0, testName), ['id'], 'success');
};

export const passMTMoveToTable = () => {
  cy.get(getElementFromAlias(getTableName(0, testName))).click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0, testName)}/browse`
  );
};

export const failMTWithoutColName = () => {
  cy.get(getElementFromAlias('add-column-button')).click();
  // cy.get('.notification-error').click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0, testName)}/modify`
  );

  validateColumn(getTableName(0, testName), [getColName(2)], 'failure');
};

export const failMTWithoutColType = () => {
  cy.get(getElementFromAlias('column-name')).type(getColName(2));
  cy.get(getElementFromAlias('add-column-button')).click();
  // cy.get('.notification-error').click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0, testName)}/modify`
  );
  validateColumn(getTableName(0, testName), [getColName(2)], 'failure');
};

export const Addcolumnnullable = () => {
  cy.get(getElementFromAlias('column-name')).type('{selectall}{del}');
  cy.get(getElementFromAlias('column-name')).type(getColName(3));
  tableColumnTypeSelector('col-type-0');
  cy.get(getElementFromAlias('data_test_column_type_value_text'))
    .first()
    .click();
  // cy.get(getElementFromAlias('data-type')).select('Text');
  cy.get(getElementFromAlias('nullable-checkbox')).uncheck({ force: true });
  cy.get(getElementFromAlias('add-column-button')).click();
  cy.wait(2500);
  // cy.get('.notification-error').click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0, testName)}/modify`
  );
  validateColumn(getTableName(0, testName), [getColName(3)], 'failure');
};

export const Addcolumnname = name => {
  cy.get(getElementFromAlias('column-name')).type('{selectall}{del}');
  cy.get(getElementFromAlias('column-name')).type(name);

  tableColumnTypeSelector('col-type-0');
  cy.get(getElementFromAlias('data_test_column_type_value_integer'))
    .first()
    .click();
  // cy.get(getElementFromAlias('data-type')).select('integer');

  cy.get(getElementFromAlias('add-column-button')).click();
  cy.wait(5000);
  validateColumn(getTableName(0, testName), [name], 'success');
};

export const passMTAddColumn = () => {
  cy.get(getElementFromAlias('column-name')).type('{selectall}{del}');
  cy.get(getElementFromAlias('column-name')).type(getColName(0));
  tableColumnTypeSelector('col-type-0');
  cy.get(getElementFromAlias('data_test_column_type_value_integer'))
    .first()
    .click();
  // cy.get(getElementFromAlias('data-type')).select('integer');
  cy.get(getElementFromAlias('add-column-button')).click();
  cy.wait(5000);
  // cy.get('.notification-success').click();
  validateColumn(getTableName(0, testName), [getColName(0)], 'success');
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
  cy.get(getElementFromAlias('edit-col-default'))
    .clear()
    .type('1234');
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
  cy.wait(5000);
  // TODO
  // test disappearance expect
  // (cy.get(getElementFromAlias('modify-table-column-1-remove'))).not.to.exist;
  cy.get(getElementFromAlias('remove-pk-column-1')).click();
  cy.get(getElementFromAlias('modify-table-pks-save')).click();
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
  cy.get(getElementFromAlias('modify-table-edit-column-1')).click();
  cy.get(getElementFromAlias('modify-table-column-1-remove')).click();
  cy.on('window:alert', str => {
    expect(str === 'Are you sure you want to delete?').to.be.true;
  });
  cy.wait(5000);
  // cy.get('.notification-success').click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0, testName)}/modify`
  );
  validateColumn(getTableName(0, testName), [getColName(0)], 'failure');
};

export const passMTDeleteTableCancel = () => {
  cy.get(getElementFromAlias('delete-table')).click();
  cy.on('window:confirm', () => false);
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0, testName)}/modify`
  );

  validateCT(getTableName(0, testName), 'success');
};

export const passMTDeleteTable = () => {
  cy.get(getElementFromAlias('delete-table')).click();
  cy.on('window:confirm', () => true);
  cy.wait(5000);
  cy.url().should('eq', `${baseUrl}/data/schema/public`);
  validateCT(getTableName(0, testName), 'failure');
};

export const setValidationMetaData = () => {
  setMetaData();
};

//  Views Modify /////////////////////////////////////////////////

export const Createtable = (name, dict) => {
  cy.url().should('eq', `${baseUrl}/data/schema/public/table/add`);
  cy.get(getElementFromAlias('tableName')).type(`${name}_table_mod`);
  const keys = Object.keys(dict).map(k => k);
  const values = Object.keys(dict).map(k => dict[k]);
  for (let i = 0; i < keys.length; i += 1) {
    cy.get('input[placeholder="column_name"]')
      .last()
      .type(keys[i]);
    cy.get('select')
      .find('option')
      .contains('-- type --')
      .parent()
      .last()
      .select(values[i]);
  }

  cy.get('select')
    .last()
    .select('id');
  cy.get(getElementFromAlias('table-create')).click();
  cy.wait(7000);
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${name}_table_mod/modify`
  );

  validateCT(`${name}_table_mod`, 'success');
};

export const Createtables = () => {
  cy.get(getElementFromAlias('data-create-table')).click();
  Createtable('author', { id: 'integer', name: 'Text' });
  cy.get(getElementFromAlias('sidebar-add-table')).click();
  Createtable('article', {
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
  validateCT('author_average_rating_mod', 'success');
};

export const Checkviewtable = () => {
  cy.get(getElementFromAlias('author_average_rating_mod')).click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/views/author_average_rating_mod/browse`
  );
  cy.get(getElementFromAlias('table-modify')).click();
  cy.get(getElementFromAlias('modify-view')).click();
  cy.url().should('eq', `${baseUrl}/data/sql`);
};

export const Checkviewtabledelete = () => {
  cy.get(getElementFromAlias('author_average_rating_mod')).click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/views/author_average_rating_mod/browse`
  );
  cy.get(getElementFromAlias('table-modify')).click();
  cy.get(getElementFromAlias('delete-view')).click();
  cy.on('window:confirm', str => {
    expect(str === 'Are you sure?').to.be.true;
    return true;
  });

  cy.wait(7000);
  validateCT('author_average_rating_mod', 'failure');
};

export const Issue = () => {
  cy.get('.ace_text-input')
    .first()
    .type('#include');
};
