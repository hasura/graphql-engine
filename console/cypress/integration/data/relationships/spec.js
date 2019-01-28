import { baseUrl, getElementFromAlias } from '../../../helpers/dataHelpers';

import {
  setMetaData,
  validateCT,
  validateColumn,
} from '../../validators/validators';

const delRel = (table, relname) => {
  cy.get(getElementFromAlias(table)).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias(`remove-button-${relname}`))
    .first()
    .click();
  cy.on('window:alert', str => {
    expect(str === 'Are you sure?').to.be.true;
  });
  cy.wait(15000);
};

export const Createtable = (name, fields) => {
  // Click on the "Add table" button and input the table name
  cy.get(getElementFromAlias('sidebar-add-table')).click();
  cy.url().should('eq', `${baseUrl}/data/schema/public/table/add`);
  cy.get(getElementFromAlias('tableName')).type(`${name}_table_rt`);

  // Enter column info
  let i = 0;
  for (const key in fields) {
    if (fields.hasOwnProperty(key)) {
      cy.get(getElementFromAlias(`column-${i}`)).type(key);
      cy.get(getElementFromAlias(`col-type-${i}`)).select(fields[key]);
      i++;
    }
  }

  // Select primary key
  cy.get(getElementFromAlias('primary-key-select-0')).select('id');
  cy.get(getElementFromAlias('table-create')).click();
  cy.wait(15000);
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${name}_table_rt/modify`
  );

  validateCT(`${name}_table_rt`, 'success');
};

export const passRTCreateTables = () => {
  Createtable('author', { id: 'Integer', name: 'Text' });
  Createtable('article', {
    id: 'Integer',
    title: 'Text',
    Content: 'Text',
    author_id: 'Integer',
    rating: 'Integer',
  });
  Createtable('comment', {
    id: 'Integer',
    user_id: 'Integer',
    article_id: 'Integer',
    comment: 'Text',
  });
};

export const passRTMoveToTable = () => {
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-relationships')).click();
};

export const Deletetable = name => {
  cy.get(getElementFromAlias(name)).click();
  cy.get(getElementFromAlias('table-modify')).click();
  cy.get(getElementFromAlias('delete-table')).click();
  cy.on('window:alert', str => {
    expect(str === 'Are you sure?').to.be.true;
  });
  cy.wait(15000);
  validateCT(name, 'failure');
};

export const passRTDeleteTables = () => {
  Deletetable('comment_table_rt');
  Deletetable('article_table_rt');
  Deletetable('author_table_rt');
};

export const passRTAddManualObjRel = () => {
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('add-manual-relationship')).click();
  cy.get(getElementFromAlias('rel-type')).select('Object Relationship');
  cy.get(getElementFromAlias('rel-name')).type('author');
  cy.get(getElementFromAlias('current-col')).select('author_id');
  cy.get(getElementFromAlias('remote-table')).select('author_table_rt');
  cy.get(getElementFromAlias('remote-table-col')).select('id');
  cy.get(getElementFromAlias('table-add-manual-relationship'))
    .last()
    .click();
  cy.wait(15000);
  validateColumn(
    'article_table_rt',
    ['title', { name: 'author', columns: ['name'] }],
    'success'
  );
};

export const passRTAddManualArrayRel = () => {
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('add-manual-relationship')).click();
  cy.get(getElementFromAlias('rel-type')).select('Array Relationship');
  cy.get(getElementFromAlias('rel-name')).type('comments');
  cy.get(getElementFromAlias('current-col')).select('id');
  cy.get(getElementFromAlias('remote-table')).select('comment_table_rt');
  cy.get(getElementFromAlias('remote-table-col')).select('article_id');
  cy.get(getElementFromAlias('table-add-manual-relationship'))
    .contains('Add')
    .last()
    .click();
  cy.wait(15000);
  validateColumn(
    'article_table_rt',
    ['title', { name: 'comments', columns: ['comment'] }],
    'success'
  );
};

export const passRTAddForeignKey = () => {
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-modify')).click();
  cy.get(getElementFromAlias('edit-author_id')).click();
  cy.get(getElementFromAlias('foreign-key-checkbox')).check();
  cy.get(getElementFromAlias('ref-table')).select('author_table_rt');
  cy.get(getElementFromAlias('ref-col')).select('id');
  cy.get(getElementFromAlias('save-button')).click();
  cy.wait(15000);
  cy.get(getElementFromAlias('comment_table_rt')).click();
  cy.get(getElementFromAlias('table-modify')).click();
  cy.get(getElementFromAlias('edit-article_id')).click();
  cy.get(getElementFromAlias('foreign-key-checkbox')).check();
  cy.get(getElementFromAlias('ref-table')).select('article_table_rt');
  cy.get(getElementFromAlias('ref-col')).select('id');
  cy.get(getElementFromAlias('save-button')).click();
  cy.wait(15000);
};

export const checkAddManualRelationshipsButton = () => {
  cy.get(getElementFromAlias('add-rel-mod')).click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/comment_table_rt/relationships`
  );
};

export const passRTDeleteRelationships = () => {
  delRel('article_table_rt', 'author');
  validateColumn(
    'article_table_rt',
    ['title', { name: 'author', columns: ['name'] }],
    'failure'
  );
  delRel('article_table_rt', 'comments');
  validateColumn(
    'article_table_rt',
    ['title', { name: 'comments', columns: ['comment'] }],
    'failure'
  );
};

export const passRTAddSuggestedRel = () => {
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('obj-rel-add-0')).click();
  cy.get(getElementFromAlias('suggested-rel-name'))
    .clear()
    .type('author');
  cy.get(getElementFromAlias('obj-rel-save-0')).click();
  cy.wait(15000);
  validateColumn(
    'article_table_rt',
    ['title', { name: 'author', columns: ['name'] }],
    'success'
  );
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('arr-rel-add-0')).click();
  cy.get(getElementFromAlias('suggested-rel-name'))
    .clear()
    .type('comments');
  cy.get(getElementFromAlias('arr-rel-save-0')).click();
  cy.wait(15000);
  validateColumn(
    'article_table_rt',
    ['title', { name: 'comments', columns: ['comment'] }],
    'success'
  );
};

export const failRTAddSuggestedRel = () => {
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('obj-rel-add-0')).click();
  cy.get(getElementFromAlias('suggested-rel-name')).clear();
  cy.get(getElementFromAlias('obj-rel-save-0')).click();
  // cy.get('.notification-error');
  cy.wait(15000);
  cy.get(getElementFromAlias('suggested-rel-name'))
    .clear()
    .type(123123);
  cy.get('button')
    .contains('Save')
    .click();
  // cy.get('.notification-error');
  cy.wait(15000);
  validateColumn(
    'article_table_rt',
    ['title', { name: 'author', columns: ['name'] }],
    'failure'
  );
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('obj-rel-add-0')).click();
  cy.get(getElementFromAlias('suggested-rel-name'))
    .clear()
    .type('author');
  cy.get(getElementFromAlias('obj-rel-save-0')).click();
  cy.wait(15000);
  validateColumn(
    'article_table_rt',
    ['title', { name: 'author', columns: ['name'] }],
    'success'
  );
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('arr-rel-add-0')).click();
  cy.get(getElementFromAlias('suggested-rel-name'))
    .clear()
    .type('author');
  cy.get(getElementFromAlias('arr-rel-save-0')).click();
  cy.wait(15000);
  // cy.get('.notification-error');
  cy.wait(15000);
  delRel('article_table_rt', 'author');
};

export const setValidationMetaData = () => {
  setMetaData();
};
