import { getElementFromAlias, baseUrl } from '../../../helpers/dataHelpers';

import {
  setMetaData,
  validateCT,
  createView,
  validateColumn,
  validateView,
} from '../../validators/validators';

const userId = 5555;

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
  cy.wait(7000);
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${name}_table/modify`
  );

  validateCT(`${name}_table`, 'success');
};

export const passVCreateTables = () => {
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

export const passVCreateViews = () => {
  createView(`CREATE VIEW author_average_rating AS
    SELECT author_table.id, avg(article_table.rating)
    From author_table, article_table
    WHERE author_table.id = article_table.author_id
    GROUP BY author_table.id`);
};

export const passTrackTable = () => {
  cy.get('a')
    .contains('Data')
    .last()
    .click();
  cy.wait(7000);
  cy.get(getElementFromAlias('add-track-table-author_average_rating')).click();
  cy.wait(5000);
  cy.get('h4').contains('Existing table/view added');
  validateView('author_average_rating', 'success');
};

export const passViewRoute = () => {
  cy.get('a')
    .contains('author_average_rating')
    .click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/views/author_average_rating/browse`
  );
};

export const passVAddDataarticle = (data, index) => {
  // Click the Insert Again button.
  cy.get('label')
    .contains('id')
    .next()
    .find('input')
    .last()
    .type('{selectall}{del}');
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
    .type('{selectall}{del}');
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
    .type('{selectall}{del}');
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
    .type('{selectall}{del}');
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
    .type('{selectall}{del}');
  cy.get('label')
    .contains('rating')
    .next()
    .find('input')
    .last()
    .type(data[4]);
  if (index) {
    cy.get('button')
      .contains('Insert Again')
      .click();
  } else {
    cy.get('button')
      .contains('Save')
      .click();
  }

  cy.wait(5000);
};

export const passVAddDataauthor = (data, index) => {
  cy.get('label')
    .contains('id')
    .next()
    .find('input')
    .last()
    .type('{selectall}{del}');
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
    .type('{selectall}{del}');
  cy.get('label')
    .contains('name')
    .next()
    .find('input')
    .last()
    .type(data[1]);
  if (index) {
    cy.get('button')
      .contains('Insert Again')
      .click();
  } else {
    cy.get('button')
      .contains('Save')
      .click();
  }
  cy.wait(5000);
};

export const passVAddDatacomment = (data, index) => {
  cy.get('label')
    .contains('id')
    .next()
    .find('input')
    .last()
    .type('{selectall}{del}');
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
    .type('{selectall}{del}');
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
    .type('{selectall}{del}');
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
    .type('{selectall}{del}');
  cy.get('label')
    .contains('comment')
    .next()
    .find('input')
    .last()
    .type(data[3]);
  if (index) {
    cy.get('button')
      .contains('Insert Again')
      .click();
  } else {
    cy.get('button')
      .contains('Save')
      .click();
  }
  cy.wait(5000);
};

const checkQuerySuccess = () => {
  // Expect only 4 rows i.e. expect fifth element to not exist
  cy.get('[role=gridcell]').contains(userId);
  cy.get('[role=row]')
    .eq(2)
    .should('not.exist');
};

export const passVAddData = () => {
  let data;
  cy.get('a')
    .contains('article_table')
    .click();
  cy.get(getElementFromAlias('table-insert-rows')).click();
  data = [1, 'A', 'Sontent', userId, 4];
  passVAddDataarticle(data, 0);
  data = [2, 'B', 'Sontenta', 2, 4];
  passVAddDataarticle(data, 1);
  data = [3, 'C', 'Sontentb', userId, 4];
  passVAddDataarticle(data, 2);
  cy.get('a')
    .contains('author_table')
    .click();
  cy.get(getElementFromAlias('table-insert-rows')).click();

  data = [userId, 'A'];
  passVAddDataauthor(data, 0);
  data = [2, 'B'];
  passVAddDataauthor(data, 1);
  cy.get('a')
    .contains('comment_table')
    .click();
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
  // Select operator as `eq`
  cy.get('select')
    .find('option')
    .contains('-- op --')
    .parent()
    .last()
    .select('$eq');
  // Type value as `filter-text`
  cy.get("input[placeholder='-- value --']")
    .last()
    .type(userId);
  // Run query
  cy.get('button')
    .contains('Run query')
    .click();
  cy.wait(5000);
  // Check if the query was successful
  checkQuerySuccess();
};

const checkOrder = order => {
  // Utility function to get right element
  const curElement = cy.get('[role=row]');
  if (order === 'asc') {
    curElement.each(($el, index) => {
      if (index === 1) {
        cy.wrap($el)
          .find('[role=gridcell]')
          .first()
          .contains(2);
      }
      if (index === 2) {
        cy.wrap($el)
          .find('[role=gridcell]')
          .first()
          .contains(userId);
      }
    });
  } else {
    curElement.each(($el, index) => {
      if (index === 2) {
        cy.wrap($el)
          .find('[role=gridcell]')
          .first()
          .contains(2);
      }
      if (index === 1) {
        cy.wrap($el)
          .find('[role=gridcell]')
          .first()
          .contains(userId);
      }
    });
  }
};

export const passVAscendingSort = () => {
  // Scroll to top TODO responsive is messy
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
  cy.get('button')
    .contains('Run query')
    .click();
  // Check order
  checkOrder('asc');
};

export const passModifyView = () => {
  cy.get('a')
    .contains('Modify')
    .click();
  cy.get('button')
    .contains('Modify')
    .last()
    .click();
  cy.url().should('eq', `${baseUrl}/data/sql`);
};

export const passVAddManualObjRel = () => {
  cy.get('a')
    .contains('author_average_rating')
    .click();
  cy.wait(2000);
  cy.get('a')
    .contains('Relationships')
    .click();
  cy.wait(2000);
  cy.get(getElementFromAlias('data-rel-type')).select('object_rel');
  cy.get("input[placeholder='Enter relationship name']").type('author');
  cy.get('select')
    .find('option')
    .contains('Current Column')
    .parent()
    .select('id');
  cy.get('select')
    .find('option')
    .contains('Remote Table')
    .parent()
    .select('author_table');
  cy.get('select')
    .last()
    .select('id');
  cy.get(getElementFromAlias('view-add-relationship')).click();
  cy.wait(7000);
  validateColumn(
    'author_average_rating',
    ['avg', { name: 'author', columns: ['name'] }],
    'success'
  );
};

export const passVDeleteRelationships = () => {
  cy.get('a')
    .contains('author_average_rating')
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
  cy.wait(5000);
  validateColumn(
    'author_average_rating',
    ['avg', { name: 'author', columns: ['name'] }],
    'failure'
  );
};

export const passVDeleteView = () => {
  cy.get('a')
    .contains('Modify')
    .click();
  cy.get('button')
    .contains('Delete view')
    .click();
  cy.on('window:confirm', str => {
    expect(str === 'Are you sure').to.be.true;
  });
  cy.wait(5000);
  cy.get('h4').contains('View deleted');
  validateView('author_average_rating', 'failure');
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
  cy.wait(7000);
};

export const passVDeleteTables = () => {
  Deletetable('comment_table');
  Deletetable('article_table');
  Deletetable('author_table');
};

// //////////////////////////////////////////////////////////////////////////////////////

export const setValidationMetaData = () => {
  setMetaData();
};
