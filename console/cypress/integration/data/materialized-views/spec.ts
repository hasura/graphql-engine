import {
  getElementFromAlias,
  baseUrl,
  tableColumnTypeSelector,
  getIndexRoute,
} from '../../../helpers/dataHelpers';

import {
  setMetaData,
  validateCT,
  createView,
  validateColumn,
  validateView,
  ResultType,
  TableFields,
} from '../../validators/validators';
import { setPromptValue } from '../../../helpers/common';

const userId = 5555;

export const createTable = (name: string, dict: TableFields) => {
  cy.url().should('eq', `${baseUrl}/data/default/schema/public/table/add`);
  cy.get(getElementFromAlias('tableName')).type(`${name}_table_vt`);
  const keys = Object.keys(dict).map(k => k);
  const values = Object.keys(dict).map(k => dict[k]);
  for (let i = 0; i < keys.length; i += 1) {
    cy.get(getElementFromAlias(`column-${i}`)).type(keys[i]);
    tableColumnTypeSelector(`col-type-${i}`);
    cy.get(getElementFromAlias(`data_test_column_type_value_${values[i]}`))
      .first()
      .click();
  }
  cy.get(getElementFromAlias('primary-key-select-0')).select('id');
  cy.get(getElementFromAlias('table-create')).click();
  cy.wait(7000);
  cy.url().should(
    'eq',
    `${baseUrl}/data/default/schema/public/tables/${name}_table_vt/modify`
  );

  validateCT(`${name}_table_vt`, ResultType.SUCCESS);
};

export const passVCreateTables = () => {
  cy.get(getElementFromAlias('data-create-table')).click();
  createTable('author', { id: 'integer', name: 'text' });
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
  cy.visit(getIndexRoute());
  cy.wait(5000);
  cy.get(getElementFromAlias('data-create-table')).click();
  createTable('comment', {
    id: 'integer',
    user_id: 'integer',
    article_id: 'integer',
    comment: 'text',
  });
};

export const passVCreateMaterializedViews = () => {
  createView(`CREATE MATERIALIZED VIEW author_average_rating_vt AS
    SELECT author_table_vt.id, avg(article_table_vt.rating)
    From author_table_vt, article_table_vt
    WHERE author_table_vt.id = article_table_vt.author_id
    GROUP BY author_table_vt.id`);
};

export const passTrackTable = () => {
  cy.visit('/data/default/schema/public/');
  cy.wait(7000);
  cy.get(
    getElementFromAlias('add-track-table-author_average_rating_vt')
  ).click();
  cy.wait(7000);
  // cy.get('.notification-error');
  validateView('author_average_rating_vt', ResultType.SUCCESS);
};

export const passMaterializedViewRoute = () => {
  cy.get(getElementFromAlias('author_average_rating_vt')).click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/default/schema/public/views/author_average_rating_vt/browse`
  );
};

export const passVAddDataarticle = (
  data: (string | number)[],
  index: number
) => {
  // Click the Insert Again button.
  cy.get('label')
    .contains('id')
    .next()
    .find('input')
    .last()
    .type('{selectall}{del}');
  cy.get('label').contains('id').next().find('input').last().type(`${data[0]}`);
  cy.get('label')
    .contains('title')
    .next()
    .find('input')
    .last()
    .type('{selectall}{del}');
  cy.get('label')
    .contains('title')
    .next()
    .find('input')
    .last()
    .type(`${data[1]}`);
  cy.get('label')
    .contains('Content')
    .next()
    .find('input')
    .last()
    .type('{selectall}{del}');
  cy.get('label')
    .contains('Content')
    .next()
    .find('input')
    .last()
    .type(`${data[2]}`);
  cy.get('label')
    .contains('author_id')
    .next()
    .find('input')
    .last()
    .type('{selectall}{del}');
  cy.get('label')
    .contains('author_id')
    .next()
    .find('input')
    .last()
    .type(`${data[3]}`);
  cy.get('label')
    .contains('rating')
    .next()
    .find('input')
    .last()
    .type('{selectall}{del}');
  cy.get('label')
    .contains('rating')
    .next()
    .find('input')
    .last()
    .type(`${data[4]}`);
  if (index) {
    cy.get(getElementFromAlias('insert-save-button')).click();
  } else {
    cy.get(getElementFromAlias('insert-save-button')).click();
  }

  cy.wait(5000);
};

export const passVAddDataauthor = (
  data: (string | number)[],
  index: number
) => {
  cy.get('label')
    .contains('id')
    .next()
    .find('input')
    .last()
    .type('{selectall}{del}');
  cy.get('label').contains('id').next().find('input').last().type(`${data[0]}`);
  cy.get('label')
    .contains('name')
    .next()
    .find('input')
    .last()
    .type('{selectall}{del}');
  cy.get('label')
    .contains('name')
    .next()
    .find('input')
    .last()
    .type(`${data[1]}`);
  if (index) {
    cy.get(getElementFromAlias('insert-save-button')).click();
  } else {
    cy.get(getElementFromAlias('insert-save-button')).click();
  }
  cy.wait(5000);
};

export const passVAddDatacomment = (
  data: (string | number)[],
  index: number
) => {
  cy.get('label')
    .contains('id')
    .next()
    .find('input')
    .last()
    .type('{selectall}{del}');
  cy.get('label').contains('id').next().find('input').last().type(`${data[0]}`);
  cy.get('label')
    .contains('user_id')
    .next()
    .find('input')
    .last()
    .type('{selectall}{del}');
  cy.get('label')
    .contains('user_id')
    .next()
    .find('input')
    .last()
    .type(`${data[1]}`);
  cy.get('label')
    .contains('article_id')
    .next()
    .find('input')
    .last()
    .type('{selectall}{del}');
  cy.get('label')
    .contains('article_id')
    .next()
    .find('input')
    .last()
    .type(`${data[2]}`);
  cy.get('label')
    .contains('comment')
    .next()
    .find('input')
    .last()
    .type('{selectall}{del}');
  cy.get('label')
    .contains('comment')
    .next()
    .find('input')
    .last()
    .type(`${data[3]}`);
  if (index) {
    cy.get(getElementFromAlias('insert-save-button')).click();
  } else {
    cy.get(getElementFromAlias('insert-save-button')).click();
  }
  cy.wait(5000);
};

const checkQuerySuccess = () => {
  // Expect only 4 rows i.e. expect fifth element to not exist
  cy.get('[role=gridcell]').contains(userId);
  cy.get('[role=row]').eq(2).should('not.exist');
};

export const passVAddData = () => {
  let data;
  cy.get(getElementFromAlias('article_table_vt')).click();
  cy.get(getElementFromAlias('table-insert-rows')).click();
  data = [1, 'A', 'Sontent', userId, 4];
  passVAddDataarticle(data, 0);
  data = [2, 'B', 'Sontenta', 2, 4];
  passVAddDataarticle(data, 1);
  data = [3, 'C', 'Sontentb', userId, 4];
  passVAddDataarticle(data, 2);
  cy.get(getElementFromAlias('author_table_vt')).click();
  cy.get(getElementFromAlias('table-insert-rows')).click();

  data = [userId, 'A'];
  passVAddDataauthor(data, 0);
  data = [2, 'B'];
  passVAddDataauthor(data, 1);
  cy.get(getElementFromAlias('comment_table_vt')).click();
  cy.get(getElementFromAlias('table-insert-rows')).click();

  data = [1, 1, 1, 'new comment'];
  passVAddDatacomment(data, 0);
  data = [2, 2, 2, 'new comment'];
  passVAddDatacomment(data, 1);
  data = [3, 1, 2, 'new comment'];
  passVAddDatacomment(data, 2);
};

export const passVFilterQueryEq = () => {
  // Select column with type `text`
  cy.get('select')
    .find('option')
    .contains('-- column --')
    .parent()
    .first()
    .select('id');
  // Type value as `filter-text`
  cy.get("input[placeholder='-- value --']").last().type(`${userId}`);
  // Run query
  cy.get(getElementFromAlias('run-query')).click();
  cy.wait(5000);
  // Check if the query was successful
  checkQuerySuccess();
};

const checkOrder = (order: string) => {
  // Utility function to get right element
  const curElement = cy.get('[role=row]');
  if (order === 'asc') {
    curElement.each(($el, index) => {
      if (index === 1) {
        cy.wrap($el).find('[role=gridcell]').first().next().next().contains(2);
      }
      if (index === 2) {
        cy.wrap($el)
          .find('[role=gridcell]')
          .first()
          .next()
          .next()
          .contains(userId);
      }
    });
  } else {
    curElement.each(($el, index) => {
      if (index === 2) {
        cy.wrap($el).find('[role=gridcell]').first().next().next().contains(2);
      }
      if (index === 1) {
        cy.wrap($el)
          .find('[role=gridcell]')
          .first()
          .next()
          .next()
          .contains(userId);
      }
    });
  }
};

export const passVAscendingSort = () => {
  cy.wait(7000);
  // cy.scrollTo('top');
  // Select column with type 'serial'
  cy.get('select')
    .find('option')
    .contains('-- column --')
    .parent()
    .last()
    .select('id');
  // Run query
  cy.get(getElementFromAlias('run-query')).click();
  // Check order
  checkOrder('asc');
};

export const passModifyMaterializedView = () => {
  cy.get(getElementFromAlias('table-modify')).click();
  cy.get('button').contains('Modify').last().click();
  cy.url().should('eq', `${baseUrl}/data/sql`);
};

export const passVAddManualObjRel = () => {
  cy.get(getElementFromAlias('author_average_rating_vt')).click();
  cy.wait(2000);
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.wait(2000);
  cy.get(getElementFromAlias('create-edit-manual-rel')).click();
  cy.get(getElementFromAlias('manual-relationship-type')).select('object');
  cy.get("input[placeholder='Enter relationship name']").type('author');
  cy.get(getElementFromAlias('manual-relationship-ref-schema')).select(
    'public'
  );
  cy.get(getElementFromAlias('manual-relationship-ref-table')).select(
    'author_table_vt'
  );
  cy.get(getElementFromAlias('manual-relationship-lcol-0')).select('id');
  cy.get(getElementFromAlias('manual-relationship-rcol-0')).select('id');
  cy.get(getElementFromAlias('create-manual-rel-save')).click();
  cy.wait(7000);
  validateColumn(
    'author_average_rating_vt',
    ['avg', { name: 'author', columns: ['name'] }],
    ResultType.SUCCESS
  );
};

export const passVDeleteRelationships = () => {
  cy.get(getElementFromAlias('author_average_rating_vt')).click();
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('relationship-toggle-editor-author')).click();
  cy.get(getElementFromAlias('relationship-remove-author')).click();
  cy.on('window:alert', str => {
    return expect(str === 'Are you sure?').to.be.true;
  });
  cy.wait(7000);
  validateColumn(
    'author_average_rating_vt',
    ['avg', { name: 'author', columns: ['name'] }],
    ResultType.FAILURE
  );
};

export const passVDeleteMaterializedView = () => {
  cy.get(getElementFromAlias('table-modify')).click();
  setPromptValue('author_average_rating_vt');
  cy.get(getElementFromAlias('delete-view')).click();
  cy.window().its('prompt').should('be.called');
  cy.wait(7000);
  // cy.get('.notification-error');
  validateView('author_average_rating_vt', ResultType.FAILURE);
};

export const deleteTable = (name: string) => {
  cy.get(getElementFromAlias(name)).click();
  cy.get(getElementFromAlias('table-modify')).click();
  setPromptValue(name);
  cy.get(getElementFromAlias('delete-table')).click();
  cy.window().its('prompt').should('be.called');
  cy.wait(7000);
  validateCT(name, ResultType.FAILURE);
  cy.wait(7000);
};

export const passVDeleteTables = () => {
  deleteTable('comment_table_vt');
  deleteTable('article_table_vt');
  deleteTable('author_table_vt');
};

export const setValidationMetaData = () => {
  setMetaData();
};
