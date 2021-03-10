import {
  baseUrl,
  getElementFromAlias,
  tableColumnTypeSelector,
  getIndexRoute,
} from '../../../helpers/dataHelpers';

import {
  setMetaData,
  validateCT,
  validateColumn,
  ResultType,
  TableFields,
} from '../../validators/validators';
import { setPromptValue } from '../../../helpers/common';

const delRel = (table: string, relname: string) => {
  cy.get(getElementFromAlias(table)).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias(`relationship-toggle-editor-${relname}`)).click();
  cy.get(getElementFromAlias(`relationship-remove-${relname}`)).click();
  cy.on('window:alert', str => {
    expect(str === 'Are you sure?').to.be.true;
  });
  cy.wait(15000);
};

export const createTable = (name: string, fields: TableFields) => {
  // Click on the "Add table" button and input the table name
  cy.visit(getIndexRoute());
  cy.wait(5000);
  cy.get(getElementFromAlias('data-create-table')).click();
  cy.url().should('eq', `${baseUrl}/data/default/schema/public/table/add`);
  cy.get(getElementFromAlias('tableName')).type(`${name}_table_rt`);

  // Enter column info
  let i = 0;
  // eslint-disable-next-line no-restricted-syntax
  for (const key in fields) {
    // eslint-disable-next-line no-prototype-builtins
    if (fields.hasOwnProperty(key)) {
      cy.get(getElementFromAlias(`column-${i}`)).type(key);
      tableColumnTypeSelector(`col-type-${i}`);
      cy.get(getElementFromAlias(`data_test_column_type_value_${fields[key]}`))
        .first()
        .click();
      i += 1;
    }
  }

  // Select primary key
  cy.get(getElementFromAlias('primary-key-select-0')).select('id');

  if (name === 'article') {
    cy.get(getElementFromAlias('add-table-edit-fk-0')).click();
    cy.get(getElementFromAlias('foreign-key-ref-table-0')).select(
      'author_table_rt'
    );
    cy.get(getElementFromAlias('foreign-key-0-lcol-0')).select('3');
    cy.get(getElementFromAlias('foreign-key-0-rcol-0')).select('id');
    cy.get(getElementFromAlias('foreign-key-0-onUpdate-cascade')).check();
    cy.get(getElementFromAlias('foreign-key-0-onDelete-cascade')).check();
  } else if (name === 'comment') {
    cy.get(getElementFromAlias('add-table-edit-fk-0')).click();
    cy.get(getElementFromAlias('foreign-key-ref-table-0')).select(
      'author_table_rt'
    );
    cy.get(getElementFromAlias('foreign-key-0-lcol-0')).select('1');
    cy.get(getElementFromAlias('foreign-key-0-rcol-0')).select('id');
    cy.get(getElementFromAlias('foreign-key-0-onUpdate-cascade')).check();
    cy.get(getElementFromAlias('foreign-key-0-onDelete-cascade')).check();
    cy.get(getElementFromAlias('add-table-edit-fk-1')).click();
    cy.get(getElementFromAlias('foreign-key-ref-table-1')).select(
      'article_table_rt'
    );
    cy.get(getElementFromAlias('foreign-key-1-lcol-0')).select('2');
    cy.get(getElementFromAlias('foreign-key-1-rcol-0')).select('id');
    cy.get(getElementFromAlias('foreign-key-1-onUpdate-cascade')).check();
    cy.get(getElementFromAlias('foreign-key-1-onDelete-cascade')).check();
  }

  cy.get(getElementFromAlias('table-create')).click();
  cy.wait(15000);
  cy.url().should(
    'eq',
    `${baseUrl}/data/default/schema/public/tables/${name}_table_rt/modify`
  );

  validateCT(`${name}_table_rt`, ResultType.SUCCESS);
};

export const passRTCreateTables = () => {
  createTable('author', { id: 'integer', name: 'text' });
  createTable('article', {
    id: 'integer',
    title: 'text',
    Content: 'text',
    author_id: 'integer',
    rating: 'integer',
  });
  createTable('comment', {
    id: 'integer',
    user_id: 'integer',
    article_id: 'integer',
    comment: 'text',
  });
};

export const Deletetable = (name: string) => {
  cy.get(getElementFromAlias(name)).click();
  cy.get(getElementFromAlias('table-modify')).click();
  setPromptValue(name);
  cy.get(getElementFromAlias('delete-table')).click();
  cy.window().its('prompt').should('be.called');
  cy.wait(15000);
  validateCT(name, ResultType.FAILURE);
};

export const passRTDeleteTables = () => {
  Deletetable('comment_table_rt');
  Deletetable('article_table_rt');
  Deletetable('author_table_rt');
};

export const passRTAddManualObjRel = () => {
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.wait(4000);
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('create-edit-manual-rel')).click();
  cy.get(getElementFromAlias('manual-relationship-type')).select('object');
  cy.get("input[placeholder='Enter relationship name']").type('author');
  cy.get(getElementFromAlias('manual-relationship-ref-schema')).select(
    'public'
  );
  cy.get(getElementFromAlias('manual-relationship-ref-table')).select(
    'author_table_rt'
  );
  cy.get(getElementFromAlias('manual-relationship-lcol-0')).select('author_id');
  cy.get(getElementFromAlias('manual-relationship-rcol-0')).select('id');
  cy.get(getElementFromAlias('create-manual-rel-save')).click();
  cy.wait(15000);
  validateColumn(
    'article_table_rt',
    ['title', { name: 'author', columns: ['name'] }],
    ResultType.SUCCESS
  );
};

export const passRTAddManualArrayRel = () => {
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.wait(4000);
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('create-edit-manual-rel')).click();
  cy.get(getElementFromAlias('manual-relationship-type')).select('array');
  cy.get("input[placeholder='Enter relationship name']").type('comments');
  cy.get(getElementFromAlias('manual-relationship-ref-schema')).select(
    'public'
  );
  cy.get(getElementFromAlias('manual-relationship-ref-table')).select(
    'comment_table_rt'
  );
  cy.get(getElementFromAlias('manual-relationship-lcol-0')).select('id');
  cy.get(getElementFromAlias('manual-relationship-rcol-0')).select(
    'article_id'
  );
  cy.get(getElementFromAlias('create-manual-rel-save')).click();
  cy.wait(15000);
  validateColumn(
    'article_table_rt',
    ['title', { name: 'comments', columns: ['comment'] }],
    ResultType.SUCCESS
  );
};

export const passRTDeleteRelationships = () => {
  delRel('article_table_rt', 'author');
  validateColumn(
    'article_table_rt',
    ['title', { name: 'author', columns: ['name'] }],
    ResultType.FAILURE
  );
  delRel('article_table_rt', 'comments');
  validateColumn(
    'article_table_rt',
    ['title', { name: 'comments', columns: ['comment'] }],
    ResultType.FAILURE
  );
};

export const passRTAddSuggestedRel = () => {
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('obj-rel-add-0')).click();
  cy.get(getElementFromAlias('suggested-rel-name')).clear().type('author');
  cy.get(getElementFromAlias('obj-rel-save-0')).click();
  cy.wait(5000);
  validateColumn(
    'article_table_rt',
    ['title', { name: 'author', columns: ['name'] }],
    ResultType.SUCCESS
  );
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.wait(5000);
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('arr-rel-add-0')).click();
  cy.get(getElementFromAlias('suggested-rel-name')).clear().type('comments');
  cy.get(getElementFromAlias('arr-rel-save-0')).click();
  cy.wait(5000);
  validateColumn(
    'article_table_rt',
    ['title', { name: 'comments', columns: ['comment'] }],
    ResultType.SUCCESS
  );
};

export const passRTRenameRelationship = () => {
  cy.get(getElementFromAlias('relationship-toggle-editor-comments')).click();
  cy.get(getElementFromAlias('relationship-name-input-comments'))
    .clear()
    .type('comments_renamed');
  cy.get(getElementFromAlias('relationship-save-comments')).click();
  cy.wait(5000);
  validateColumn(
    'article_table_rt',
    ['title', { name: 'comments_renamed', columns: ['comment'] }],
    ResultType.SUCCESS
  );
  cy.get(
    getElementFromAlias('relationship-toggle-editor-comments_renamed')
  ).click();
  cy.get(getElementFromAlias('relationship-name-input-comments_renamed'))
    .clear()
    .type('comments');
  cy.get(getElementFromAlias('relationship-save-comments_renamed')).click();
  cy.wait(5000);
  validateColumn(
    'article_table_rt',
    ['title', { name: 'comments', columns: ['comment'] }],
    ResultType.SUCCESS
  );
};

export const failRTAddSuggestedRel = () => {
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('obj-rel-add-0')).click();
  cy.get(getElementFromAlias('suggested-rel-name')).clear();
  cy.get(getElementFromAlias('obj-rel-save-0')).click();
  cy.wait(15000);
  cy.get(getElementFromAlias('suggested-rel-name')).clear().type(`${123123}`);
  cy.get('button').contains('Save').click();
  cy.wait(15000);
  validateColumn(
    'article_table_rt',
    ['title', { name: 'author', columns: ['name'] }],
    ResultType.FAILURE
  );
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('obj-rel-add-0')).click();
  cy.get(getElementFromAlias('suggested-rel-name')).clear().type('author');
  cy.get(getElementFromAlias('obj-rel-save-0')).click();
  cy.wait(15000);
  validateColumn(
    'article_table_rt',
    ['title', { name: 'author', columns: ['name'] }],
    ResultType.SUCCESS
  );
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('arr-rel-add-0')).click();
  cy.get(getElementFromAlias('suggested-rel-name')).clear().type('comments');
  cy.get(getElementFromAlias('arr-rel-save-0')).click();
  cy.wait(15000);
  delRel('article_table_rt', 'author');
  delRel('article_table_rt', 'comments');
};

export const setValidationMetaData = () => {
  setMetaData();
};
