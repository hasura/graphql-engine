import { baseUrl, getElementFromAlias } from '../../../helpers/dataHelpers';

import {
  setMetaData,
  validateCT,
  validateColumn,
} from '../../validators/validators';

export const Createtable = (name, dict) => {
  cy.url().should('eq', `${baseUrl}/data/schema/public/table/add`);
  cy.get(getElementFromAlias('tableName')).type(`${name}_table_rt`);
  const keys = Object.keys(dict).map(k => k);
  const values = Object.keys(dict).map(k => dict[k]);
  for (let i = 0; i < keys.length; i += 1) {
    cy.get(getElementFromAlias(`column-${i}`)).type(keys[i]);
    cy.get(getElementFromAlias(`col-type-${i}`)).select(values[i]);
  }

  cy.get(getElementFromAlias('primary-key-select-0')).select('id');
  cy.get(getElementFromAlias('table-create')).click();
  cy.wait(10000);
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${name}_table/modify`
  );

  validateCT(`${name}_table`, 'success');
};

export const passRTCreateTables = () => {
  cy.get(getElementFromAlias('data-create-table')).click();
  Createtable('author', { id: 'Integer', name: 'Text' });
  cy.get(getElementFromAlias('sidebar-add-table')).click();
  Createtable('article', {
    id: 'Integer',
    title: 'Text',
    Content: 'Text',
    author_id: 'Integer',
    rating: 'Integer',
  });
  cy.get(getElementFromAlias('sidebar-add-table')).click();
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
  cy.wait(7000);
  validateCT(name, 'failure');
};

export const passRTDeleteTables = () => {
  Deletetable('comment_table_rt');
  Deletetable('article_table_rt');
  Deletetable('author_table_rt');
};

export const passRTAddDataarticle = data => {
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-insert-rows')).click();
  cy.get('label')
    .contains('id')
    .next()
    .find('input')
    .last()
    .type(data[0]);
  cy.get('label')
    .contains('title')
    .next()
    .find('input')
    .last()
    .type(data[1]);
  cy.get('label')
    .contains('Content')
    .next()
    .find('input')
    .last()
    .type(data[2]);
  cy.get('label')
    .contains('author_id')
    .next()
    .find('input')
    .last()
    .type(data[3]);
  cy.get('label')
    .contains('rating')
    .next()
    .find('input')
    .last()
    .type(data[4]);
  cy.get('button')
    .contains('Save')
    .click();
  cy.wait(7000);
};

export const passRTAddDataauthor = data => {
  cy.get(getElementFromAlias('author_table_rt')).click();
  cy.get(getElementFromAlias('table-insert-rows')).click();
  cy.get('label')
    .contains('id')
    .next()
    .find('input')
    .last()
    .type(data[0]);
  cy.get('label')
    .contains('name')
    .next()
    .find('input')
    .last()
    .type(data[1]);
  cy.get(getElementFromAlias('insert-save-button')).click();
  cy.wait(7000);
};

export const passRTAddDatacomment = data => {
  cy.get(getElementFromAlias('comment_table_rt')).click();
  cy.get(getElementFromAlias('table-insert-rows')).click();
  cy.get('label')
    .contains('id')
    .next()
    .find('input')
    .last()
    .type(data[0]);
  cy.get('label')
    .contains('user_id')
    .next()
    .find('input')
    .last()
    .type(data[1]);
  cy.get('label')
    .contains('article_id')
    .next()
    .find('input')
    .last()
    .type(data[2]);
  cy.get('label')
    .contains('comment')
    .next()
    .find('input')
    .last()
    .type(data[3]);
  cy.get(getElementFromAlias('insert-save-button')).click();
  cy.wait(7000);
};

export const passRTAddData = () => {
  let data;
  data = [1, 'A', 'Sontent', 1, 4];
  passRTAddDataarticle(data);
  data = [2, 'B', 'Sontenta', 2, 4];
  passRTAddDataarticle(data);
  data = [3, 'C', 'Sontentb', 1, 4];
  passRTAddDataarticle(data);
  data = [1, 'A'];
  passRTAddDataauthor(data);
  data = [2, 'B'];
  passRTAddDataauthor(data);
  data = [1, 1, 1, 'new comment'];
  passRTAddDatacomment(data);
  data = [2, 2, 2, 'new comment'];
  passRTAddDatacomment(data);
  data = [3, 1, 2, 'new comment'];
  passRTAddDatacomment(data);
};

export const passRTAddManualObjRel = () => {
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('add-manual-relationship')).click();
  cy.get('select')
    .first()
    .select('Object Relationship');
  cy.get("input[placeholder='Enter relationship name']").type('author');
  cy.get('select')
    .find('option')
    .contains('Current Column')
    .parent()
    .select('author_id');
  cy.get('select')
    .find('option')
    .contains('Remote Table')
    .parent()
    .select('author_table_rt');
  cy.get('select')
    .last()
    .select('id');
  cy.get(getElementFromAlias('table-add-manual-relationship'))
    .contains('Add')
    .last()
    .click();
  cy.wait(7000);
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
  cy.get('select')
    .first()
    .select('Array Relationship');
  cy.get("input[placeholder='Enter relationship name']").type('comments');
  cy.get('select')
    .find('option')
    .contains('Current Column')
    .parent()
    .select('id');
  cy.get('select')
    .find('option')
    .contains('Remote Table')
    .parent()
    .select('comment_table_rt');
  cy.get('select')
    .last()
    .select('article_id');
  cy.get(getElementFromAlias('table-add-manual-relationship'))
    .contains('Add')
    .last()
    .click();
  cy.wait(7000);
  validateColumn(
    'article_table_rt',
    ['title', { name: 'comments', columns: ['comment'] }],
    'success'
  );
};

export const passRTAddForeignKey = () => {
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-modify')).click();
  cy.get('b')
    .contains('author_id')
    .prev()
    .click();
  cy.get('[data-test=foreign-key-checkbox]').check();
  cy.get('select')
    .find('option')
    .contains('Reference table')
    .parent()
    .select('author_table_rt');
  cy.get('select')
    .find('option')
    .contains('Reference column')
    .parent()
    .select('id');
  cy.get(getElementFromAlias('save-button')).click();
  cy.wait(7000);
  cy.get(getElementFromAlias('comment_table_rt')).click();
  cy.get(getElementFromAlias('table-modify')).click();
  cy.get('b')
    .contains('article_id')
    .prev()
    .click();
  cy.get('[data-test=foreign-key-checkbox]').check();
  cy.get('select')
    .find('option')
    .contains('Reference table')
    .parent()
    .select('article_table_rt');
  cy.get('select')
    .find('option')
    .contains('Reference column')
    .parent()
    .select('id');
  cy.get(getElementFromAlias('save-button')).click();
  cy.wait(7000);
};

export const passRTDeleteRelationships = () => {
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('remove-button'))
    .first()
    .click();
  cy.on('window:alert', str => {
    expect(str === 'Are you sure?').to.be.true;
  });
  cy.wait(7000);
  validateColumn(
    'article_table_rt',
    ['title', { name: 'author', columns: ['name'] }],
    'failure'
  );
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('remove-button'))
    .first()
    .click();
  cy.on('window:alert', str => {
    expect(str === 'Are you sure?').to.be.true;
  });
  cy.wait(7000);
  validateColumn(
    'article_table_rt',
    ['title', { name: 'comments', columns: ['comment'] }],
    'failure'
  );
};

export const passRTAddSuggestedRel = () => {
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('obj-rel-add-0'))
    .contains('Add')
    .last()
    .click();
  cy.get('input')
    .last()
    .type('author');
  cy.get(getElementFromAlias('obj-rel-save-0')).click();
  cy.wait(7000);
  validateColumn(
    'article_table_rt',
    ['title', { name: 'author', columns: ['name'] }],
    'success'
  );
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('arr-rel-add-0'))
    .contains('Add')
    .last()
    .click();
  cy.get('input')
    .last()
    .type('comments');
  cy.get(getElementFromAlias('arr-rel-save-0')).click();
  cy.wait(7000);
  validateColumn(
    'article_table_rt',
    ['title', { name: 'comments', columns: ['comment'] }],
    'success'
  );
};

export const failRTAddSuggestedRel = () => {
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('obj-rel-add-0'))
    .contains('Add')
    .last()
    .click();
  cy.get(getElementFromAlias('obj-rel-save-0')).click();
  cy.get('.notification-error');
  cy.wait(7000);
  cy.get('input')
    .last()
    .type(123123);
  cy.get('button')
    .contains('Save')
    .click();
  cy.get('.notification-error');
  cy.wait(10000);
  validateColumn(
    'article_table_rt',
    ['title', { name: 'author', columns: ['name'] }],
    'failure'
  );
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('obj-rel-add-0'))
    .last()
    .click();
  cy.get('input')
    .last()
    .type('author');
  cy.get(getElementFromAlias('obj-rel-save-0')).click();
  cy.wait(10000);
  validateColumn(
    'article_table_rt',
    ['title', { name: 'author', columns: ['name'] }],
    'success'
  );
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('arr-rel-add-0'))
    .contains('Add')
    .last()
    .click();
  cy.get('input')
    .last()
    .type('author');
  cy.get('button')
    .contains('Save')
    .click();
  cy.wait(7000);
  cy.get('.notification-error');
  cy.wait(7000);
  cy.get(getElementFromAlias('article_table_rt')).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('remove-button'))
    .first()
    .click();
  cy.on('window:alert', str => {
    expect(str === 'Are you sure?').to.be.true;
  });
  cy.wait(7000);
};

export const setValidationMetaData = () => {
  setMetaData();
};
