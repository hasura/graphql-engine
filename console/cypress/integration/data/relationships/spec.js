import { baseUrl, getElementFromAlias } from '../../../helpers/dataHelpers';

import {
  setMetaData,
  validateCT,
  validateColumn,
} from '../../validators/validators';

export const Createtable = (name, dict) => {
  cy.url().should('eq', `${baseUrl}/data/schema/public/table/add`);
  cy.get(getElementFromAlias('tableName')).type(`${name}_table`);
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
  cy.get('button')
    .contains('Create')
    .click();
  cy.wait(10000);
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${name}_table/modify`
  );

  validateCT(`${name}_table`, 'success');
};

export const passRTCreateTables = () => {
  cy.get('button')
    .contains('Create Table')
    .click();
  Createtable('author', { id: 'Integer', name: 'Text' });
  cy.get('button')
    .contains('Add Table')
    .click();
  Createtable('article', {
    id: 'Integer',
    title: 'Text',
    Content: 'Text',
    author_id: 'Integer',
    rating: 'Integer',
  });
  cy.get('button')
    .contains('Add Table')
    .click();
  Createtable('comment', {
    id: 'Integer',
    user_id: 'Integer',
    article_id: 'Integer',
    comment: 'Text',
  });
};

export const passRTMoveToTable = () => {
  cy.get('a')
    .contains('article_table')
    .click();
  cy.get('a')
    .contains('Relationships')
    .click();
};

export const Deletetable = name => {
  cy.get('a')
    .contains(name)
    .click();
  cy.get('a')
    .contains('Modify')
    .click();
  cy.get('button')
    .contains('Delete table')
    .click();
  cy.on('window:alert', str => {
    expect(str === 'Are you sure?').to.be.true;
  });
  cy.wait(7000);
  validateCT(name, 'failure');
};

export const passRTDeleteTables = () => {
  Deletetable('comment_table');
  Deletetable('article_table');
  Deletetable('author_table');
};

export const passRTAddDataarticle = data => {
  cy.get('a')
    .contains('article_table')
    .click();
  cy.get('a')
    .contains('Insert Row')
    .click();
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
  cy.get('a')
    .contains('author_table')
    .click();
  cy.get('a')
    .contains('Insert Row')
    .click();
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
  cy.get('button')
    .contains('Save')
    .click();
  cy.wait(7000);
};

export const passRTAddDatacomment = data => {
  cy.get('a')
    .contains('comment_table')
    .click();
  cy.get('a')
    .contains('Insert Row')
    .click();
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
  cy.get('button')
    .contains('Save')
    .click();
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
  cy.get('a')
    .contains('article_table')
    .click();
  cy.get('a')
    .contains('Relationships')
    .click();
  cy.get('button')
    .contains('+ Add a manual relationship')
    .click();
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
    .select('author_table');
  cy.get('select')
    .last()
    .select('id');
  cy.get(getElementFromAlias('table-add-manual-relationship'))
    .contains('Add')
    .last()
    .click();
  cy.wait(7000);
  validateColumn(
    'article_table',
    ['title', { name: 'author', columns: ['name'] }],
    'success'
  );
};

export const passRTAddManualArrayRel = () => {
  cy.get('a')
    .contains('article_table')
    .click();
  cy.get('a')
    .contains('Relationships')
    .click();
  cy.get('button')
    .contains('+ Add a manual relationship')
    .click();
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
    .select('comment_table');
  cy.get('select')
    .last()
    .select('article_id');
  cy.get(getElementFromAlias('table-add-manual-relationship'))
    .contains('Add')
    .last()
    .click();
  cy.wait(7000);
  validateColumn(
    'article_table',
    ['title', { name: 'comments', columns: ['comment'] }],
    'success'
  );
};

export const passRTAddForeignKey = () => {
  cy.get('a')
    .contains('article_table')
    .click();
  cy.get('a')
    .contains('Modify')
    .click();
  cy.get('b')
    .contains('author_id')
    .prev()
    .click();
  cy.get('[data-test=foreign-key-checkbox]').check();
  cy.get('select')
    .find('option')
    .contains('Reference table')
    .parent()
    .select('author_table');
  cy.get('select')
    .find('option')
    .contains('Reference column')
    .parent()
    .select('id');
  cy.get('button')
    .contains('Save')
    .click();
  cy.wait(7000);
  cy.get('a')
    .contains('comment_table')
    .click();
  cy.get('a')
    .contains('Modify')
    .click();
  cy.get('b')
    .contains('article_id')
    .prev()
    .click();
  cy.get('[data-test=foreign-key-checkbox]').check();
  cy.get('select')
    .find('option')
    .contains('Reference table')
    .parent()
    .select('article_table');
  cy.get('select')
    .find('option')
    .contains('Reference column')
    .parent()
    .select('id');
  cy.get('button')
    .contains('Save')
    .click();
  cy.wait(7000);
};

export const passRTDeleteRelationships = () => {
  cy.get('a')
    .contains('article_table')
    .click();
  cy.get('a')
    .contains('Relationships')
    .click();
  cy.get('button')
    .contains('Remove')
    .first()
    .click();
  cy.on('window:alert', str => {
    expect(str === 'Are you sure?').to.be.true;
  });
  cy.wait(7000);
  validateColumn(
    'article_table',
    ['title', { name: 'author', columns: ['name'] }],
    'failure'
  );
  cy.get('a')
    .contains('article_table')
    .click();
  cy.get('a')
    .contains('Relationships')
    .click();
  cy.get('button')
    .contains('Remove')
    .first()
    .click();
  cy.on('window:alert', str => {
    expect(str === 'Are you sure?').to.be.true;
  });
  cy.wait(7000);
  validateColumn(
    'article_table',
    ['title', { name: 'comments', columns: ['comment'] }],
    'failure'
  );
};

export const passRTAddSuggestedRel = () => {
  cy.get('a')
    .contains('article_table')
    .click();
  cy.get('a')
    .contains('Relationships')
    .click();
  cy.get(getElementFromAlias('obj-rel-add-0'))
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
  validateColumn(
    'article_table',
    ['title', { name: 'author', columns: ['name'] }],
    'success'
  );
  cy.get('a')
    .contains('article_table')
    .click();
  cy.get('a')
    .contains('Relationships')
    .click();
  cy.get(getElementFromAlias('arr-rel-add-0'))
    .contains('Add')
    .last()
    .click();
  cy.get('input')
    .last()
    .type('comments');
  cy.get('button')
    .contains('Save')
    .click();
  cy.wait(7000);
  validateColumn(
    'article_table',
    ['title', { name: 'comments', columns: ['comment'] }],
    'success'
  );
};

export const failRTAddSuggestedRel = () => {
  cy.get('a')
    .contains('article_table')
    .click();
  cy.get('a')
    .contains('Relationships')
    .click();
  cy.get(getElementFromAlias('obj-rel-add-0'))
    .contains('Add')
    .last()
    .click();
  cy.get('button')
    .contains('Save')
    .click();
  cy.get('h4').contains('Error adding relationship!');
  cy.wait(7000);
  cy.get('input')
    .last()
    .type(123123);
  cy.get('button')
    .contains('Save')
    .click();
  cy.get('h4').contains('Error adding relationship!');
  cy.wait(10000);
  validateColumn(
    'article_table',
    ['title', { name: 'author', columns: ['name'] }],
    'failure'
  );
  cy.get('a')
    .contains('article_table')
    .click();
  cy.get('a')
    .contains('Relationships')
    .click();
  cy.get(getElementFromAlias('obj-rel-add-0'))
    .contains('Add')
    .last()
    .click();
  cy.get('input')
    .last()
    .type('author');
  cy.get('button')
    .contains('Save')
    .click();
  cy.wait(10000);
  validateColumn(
    'article_table',
    ['title', { name: 'author', columns: ['name'] }],
    'success'
  );
  cy.get('a')
    .contains('article_table')
    .click();
  cy.get('a')
    .contains('Relationships')
    .click();
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
  cy.get('h4').contains('Creating relationship failed');
  cy.wait(7000);
  cy.get('a')
    .contains('article_table')
    .click();
  cy.get('a')
    .contains('Relationships')
    .click();
  cy.get('button')
    .contains('Remove')
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
