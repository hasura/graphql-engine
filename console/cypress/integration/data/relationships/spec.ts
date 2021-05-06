import { baseUrl, tableColumnTypeSelector } from '../../../helpers/dataHelpers';

import {
  validateCT,
  validateColumn,
  ResultType,
  TableFields,
} from '../../validators/validators';

const delRel = (table: string, relname: string) => {
  cy.getBySel(table).click();
  cy.getBySel('table-relationships').click();
  cy.getBySel(`relationship-toggle-editor-${relname}`).click();
  cy.getBySel(`relationship-remove-${relname}`).click();
  cy.on('window:alert', str => {
    expect(str === 'Are you sure?').to.be.true;
  });
  cy.wait(1000);
  return cy.checkNotification('Relationship deleted', { timeout: 10000 });
};

export const createTable = (name: string, fields: TableFields) => {
  cy.getBySel('public_schema').click();
  cy.getBySel('data-create-table').click();
  cy.url().should('eq', `${baseUrl}/data/default/schema/public/table/add`);
  cy.getBySel('tableName').type(`${name}_table_rt`);

  // Enter column info
  let i = 0;
  // eslint-disable-next-line no-restricted-syntax
  for (const key in fields) {
    // eslint-disable-next-line no-prototype-builtins
    if (fields.hasOwnProperty(key)) {
      cy.getBySel(`column-${i}`).type(key);
      tableColumnTypeSelector(`col-type-${i}`);
      cy.getBySel(`data_test_column_type_value_${fields[key]}`).first().click();
      i += 1;
    }
  }

  // Select primary key
  cy.getBySel('primary-key-select-0').select('id');

  if (name === 'article') {
    cy.getBySel('add-table-edit-fk-0').click();
    cy.getBySel('foreign-key-ref-table-0').select('author_table_rt');
    cy.getBySel('foreign-key-0-lcol-0').select('3');
    cy.getBySel('foreign-key-0-rcol-0').select('id');
    cy.getBySel('foreign-key-0-onUpdate-cascade').check();
    cy.getBySel('foreign-key-0-onDelete-cascade').check();
  } else if (name === 'comment') {
    cy.getBySel('add-table-edit-fk-0').click();
    cy.getBySel('foreign-key-ref-table-0').select('author_table_rt');
    cy.getBySel('foreign-key-0-lcol-0').select('1');
    cy.getBySel('foreign-key-0-rcol-0').select('id');
    cy.getBySel('foreign-key-0-onUpdate-cascade').check();
    cy.getBySel('foreign-key-0-onDelete-cascade').check();
    cy.getBySel('add-table-edit-fk-1').click();
    cy.getBySel('foreign-key-ref-table-1').select('article_table_rt');
    cy.getBySel('foreign-key-1-lcol-0').select('2');
    cy.getBySel('foreign-key-1-rcol-0').select('id');
    cy.getBySel('foreign-key-1-onUpdate-cascade').check();
    cy.getBySel('foreign-key-1-onDelete-cascade').check();
  }

  cy.getBySel('table-create').click();
  cy.url({ timeout: 15000 }).should(
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
  cy.getBySel(name).click();
  cy.getBySel('table-modify').click();
  cy.setPrompt(name, () => {
    cy.getBySel('delete-table').click();
    cy.window().its('prompt').should('be.called');
    cy.wait(1000);
    cy.checkNotification('Table deleted', { timeout: 10000 }).then(() => {
      validateCT(name, ResultType.FAILURE);
    });
  });
};

export const passRTDeleteTables = () => {
  Deletetable('comment_table_rt');
  Deletetable('article_table_rt');
  Deletetable('author_table_rt');
};

export const passRTAddManualObjRel = () => {
  cy.getBySel('article_table_rt').click();
  cy.getBySel('table-relationships', { timeout: 4000 }).click();
  cy.getBySel('article_table_rt').click();
  cy.getBySel('table-relationships').click();
  cy.getBySel('create-edit-manual-rel').click();
  cy.getBySel('manual-relationship-type').select('object');
  cy.get("input[placeholder='Enter relationship name']").type('author');
  cy.getBySel('manual-relationship-ref-schema').select('public');
  cy.getBySel('manual-relationship-ref-table').select('author_table_rt');
  cy.getBySel('manual-relationship-lcol-0').select('author_id');
  cy.getBySel('manual-relationship-rcol-0').select('id');
  cy.getBySel('create-manual-rel-save').click();
  cy.wait(1000);
  cy.checkNotification('Relationship created', { timeout: 10000 });
  validateColumn(
    'article_table_rt',
    ['title', { name: 'author', columns: ['name'] }],
    ResultType.SUCCESS
  );
};

export const passRTAddManualArrayRel = () => {
  cy.getBySel('article_table_rt').click();
  cy.getBySel('table-relationships', { timeout: 4000 }).click();
  cy.getBySel('create-edit-manual-rel').click();
  cy.getBySel('manual-relationship-type').select('array');
  cy.get("input[placeholder='Enter relationship name']").type('comments');
  cy.getBySel('manual-relationship-ref-schema').select('public');
  cy.getBySel('manual-relationship-ref-table').select('comment_table_rt');
  cy.getBySel('manual-relationship-lcol-0').select('id');
  cy.getBySel('manual-relationship-rcol-0').select('article_id');
  cy.getBySel('create-manual-rel-save').click();
  cy.wait(1000);
  cy.checkNotification('Relationship created', { timeout: 10000 }).then(() => {
    validateColumn(
      'article_table_rt',
      ['title', { name: 'comments', columns: ['comment'] }],
      ResultType.SUCCESS
    );
  });
};

export const passRTDeleteRelationships = () => {
  delRel('article_table_rt', 'author').then(() => {
    validateColumn(
      'article_table_rt',
      ['title', { name: 'author', columns: ['name'] }],
      ResultType.FAILURE
    );
  });
  delRel('article_table_rt', 'comments').then(() => {
    validateColumn(
      'article_table_rt',
      ['title', { name: 'comments', columns: ['comment'] }],
      ResultType.FAILURE
    );
  });
};

export const passRTAddSuggestedRel = () => {
  cy.getBySel('article_table_rt').click();
  cy.getBySel('table-relationships').click();
  cy.getBySel('obj-rel-add-0').click();
  cy.getBySel('suggested-rel-name').clear().type('author');
  cy.getBySel('obj-rel-save-0').click();
  cy.wait(1000);
  cy.checkNotification('Relationship created', { timeout: 10000 });
  validateColumn(
    'article_table_rt',
    ['title', { name: 'author', columns: ['name'] }],
    ResultType.SUCCESS
  );
  cy.getBySel('article_table_rt').click();
  cy.getBySel('table-relationships', { timeout: 5000 }).click();
  cy.getBySel('arr-rel-add-0').click();
  cy.getBySel('suggested-rel-name').clear().type('comments');
  cy.getBySel('arr-rel-save-0').click({ force: true });
  cy.wait(1000);
  cy.checkNotification('Relationship created', { timeout: 10000 });
  validateColumn(
    'article_table_rt',
    ['title', { name: 'comments', columns: ['comment'] }],
    ResultType.SUCCESS
  );
};

export const passRTRenameRelationship = () => {
  cy.getBySel('relationship-toggle-editor-comments').click();
  cy.getBySel('relationship-name-input-comments')
    .clear()
    .type('comments_renamed');
  cy.getBySel('relationship-save-comments').click();
  cy.wait(1000);
  cy.checkNotification('Relationship renamed', { timeout: 10000 });
  validateColumn(
    'article_table_rt',
    ['title', { name: 'comments_renamed', columns: ['comment'] }],
    ResultType.SUCCESS
  );
  cy.getBySel('relationship-toggle-editor-comments_renamed').click();
  cy.getBySel('relationship-name-input-comments_renamed')
    .clear()
    .type('comments');
  cy.getBySel('relationship-save-comments_renamed').click();
  cy.wait(1000);
  cy.checkNotification('Relationship renamed', { timeout: 10000 });
  validateColumn(
    'article_table_rt',
    ['title', { name: 'comments', columns: ['comment'] }],
    ResultType.SUCCESS
  );
};

export const failRTAddSuggestedRel = () => {
  cy.getBySel('article_table_rt').click();
  cy.getBySel('table-relationships').click();
  cy.getBySel('obj-rel-add-0').click();
  cy.getBySel('suggested-rel-name').clear();
  cy.getBySel('obj-rel-save-0').click();
  cy.checkNotification('Relationship name cannot be empty', {
    timeout: 10000,
    type: 'error',
  });
  cy.getBySel('suggested-rel-name').type(`${123123}`);
  cy.get('button').contains('Save').click();
  cy.checkNotification('Relationship name cannot contain special characters', {
    timeout: 10000,
    type: 'error',
  });
  validateColumn(
    'article_table_rt',
    ['title', { name: 'author', columns: ['name'] }],
    ResultType.FAILURE
  );
  cy.getBySel('article_table_rt').click();
  cy.getBySel('table-relationships').click();
  cy.getBySel('obj-rel-add-0').click();
  cy.getBySel('suggested-rel-name').clear().type('author');
  cy.getBySel('obj-rel-save-0').click();
  cy.wait(1000);
  cy.checkNotification('Relationship created', { timeout: 10000 }).then(() => {
    validateColumn(
      'article_table_rt',
      ['title', { name: 'author', columns: ['name'] }],
      ResultType.SUCCESS
    );
  });
  cy.getBySel('article_table_rt').click();
  cy.getBySel('table-relationships').click();
  cy.getBySel('arr-rel-add-0').click();
  cy.getBySel('suggested-rel-name').clear().type('comments');
  cy.getBySel('arr-rel-save-0').click();
  cy.wait(1000);
  cy.checkNotification('Relationship created', { timeout: 10000 });
  delRel('article_table_rt', 'author');
  delRel('article_table_rt', 'comments');
};
