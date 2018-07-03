import {
  baseUrl,
  getTableName,
  getElementFromAlias,
} from '../../../helpers/dataHelpers';

import {
  setMetaData,
  validateCT,
  validateColumn,
} from '../../validators/validators';

export const passMTCreateTable = () => {
  cy.get('button')
    .contains('Create Table')
    .click();
  cy.url().should('eq', `${baseUrl}/data/schema/public/table/add`);
  cy.get(getElementFromAlias('tableName')).type(getTableName(0));
  cy.get('input[placeholder="column_name"]').type('id');
  cy.get('select')
    .first()
    .select('Integer');
  cy.get('select')
    .last()
    .select('id');
  cy.get('button')
    .contains('Create')
    .click();
  cy.wait(7000);
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0)}/modify`
  );

  validateCT(getTableName(0), 'success');
};

export const makeid = () => {
  let text = '';
  const possible = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';

  for (let i = 0; i < 5; i += 1) {
    text += possible.charAt(Math.floor(Math.random() * possible.length));
  }

  return text;
};

export const passMTCheckRoute = () => {
  // Click on the create table button
  cy.get('a')
    .contains('Modify')
    .click();
  // Match the URL
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0)}/modify`
  );
};

export const passMTMoveToTable = () => {
  cy.get('a')
    .contains(getTableName(0))
    .click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0)}/browse`
  );
};

export const failMTWithoutColName = () => {
  cy.get('button')
    .contains('+ Add column')
    .click();
  cy.get('[class=notification-title]')
    .contains('Error adding column!')
    .click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0)}/modify`
  );

  validateColumn(getTableName(0), ['something'], 'failure');
};

export const failMTWithoutColType = () => {
  cy.get('input[placeholder = "column name"]').type('something');
  cy.get('button')
    .contains('+ Add column')
    .click();
  cy.get('[class=notification-title]')
    .contains('Error creating column!')
    .click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0)}/modify`
  );
  validateColumn(getTableName(0), ['something'], 'failure');
};

export const failMTDuplicateColumns = () => {
  cy.get('input[placeholder = "column name"]').type('{selectall}{del}');
  cy.get('input[placeholder = "column name"]').type('id');
  cy.get('select').select('integer');
  cy.get('button')
    .contains('+ Add column')
    .click();
  cy.wait(5000);
  cy.get('[class=notification-title]')
    .contains('Adding column failed')
    .click();
};

export const Addcolumn = () => {
  cy.get('input[placeholder = "column name"]').type('{selectall}{del}');
  const name = makeid();
  cy.get('input[placeholder = "column name"]').type(name);
  cy.get(getElementFromAlias('data-type')).select('integer');
  cy.get('input[placeholder = "default value"]').type('{selectall}{del}');
  cy.get('[data-test=nullable-checkbox]').check();
  cy.get('button')
    .contains('+ Add column')
    .click();
  cy.wait(5000);
  cy.get('[class=notification-title]').contains('Column added');
  cy.wait(5000);

  validateColumn(getTableName(0), [name], 'success');
};

export const Addcolumnnullable = () => {
  cy.get('input[placeholder = "column name"]').type('{selectall}{del}');
  cy.get('input[placeholder = "column name"]').type('some');
  cy.get('select').select('Text');
  cy.get('[data-test=nullable-checkbox]').uncheck();
  cy.get('button')
    .contains('+ Add column')
    .click();
  cy.wait(5000);
  cy.get('[class=notification-title]')
    .contains('Adding column failed')
    .click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0)}/modify`
  );
  validateColumn(getTableName(0), ['some'], 'failure');
};

export const failMTWrongDefault = () => {
  cy.get('input[placeholder = "column name"]').type('{selectall}{del}');
  cy.get('input[placeholder = "column name"]').type('some');
  cy.get('input[placeholder = "default value"]').type('some');
  cy.get('select').select('Integer');
  cy.get('button')
    .contains('+ Add column')
    .click();
  cy.wait(5000);
  cy.get('[class=notification-title]').contains('Adding column failed');
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0)}/modify`
  );
  validateColumn(getTableName(0), ['some'], 'failure');
};

export const Addcolumnname = name => {
  cy.get('input[placeholder = "column name"]').type('{selectall}{del}');
  cy.get('input[placeholder = "column name"]').type(name);
  cy.get('select').select('integer');

  cy.get('button')
    .contains('+ Add column')
    .click();
  cy.wait(5000);
  cy.get('[class=notification-title]')
    .contains('Column added')
    .click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0)}/modify`
  );
  validateColumn(getTableName(0), [name], 'success');
};

export const passMTAddTwoCols = () => {
  Addcolumn();
  Addcolumn();
};

export const Movetocolumn = () => {
  Addcolumnname('somes');
  cy.get('h5')
    .contains('somes')
    .parent()
    .contains('Edit')
    .click();
};

export const passMTChangeColType = () => {
  Movetocolumn();
  cy.get('.form-horizontal')
    .children()
    .get('select')
    .first()
    .select('Text');
  cy.get('button')
    .contains('Save')
    .click();
  cy.wait(5000);
  cy.get('[class=notification-title]')
    .contains('Column modified')
    .click();
  cy.get('button')
    .contains('Close')
    .click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0)}/modify`
  );
  validateColumn(getTableName(0), ['somes'], 'success');
};

export const passMTDeleteCol = () => {
  cy.get('h5')
    .contains('somes')
    .parent()
    .contains('Edit')
    .click();
  cy.get('button')
    .contains('Remove')
    .click();
  cy.on('window:alert', str => {
    expect(str === 'Are you sure you want to delete?').to.be.true;
  });
  cy.wait(5000);
  cy.get('[class=notification-title]')
    .contains('Column deleted')
    .click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0)}/modify`
  );
  validateColumn(getTableName(0), ['somes'], 'failure');
};

export const passMTDeleteTableCancel = () => {
  cy.get('button')
    .contains('Delete table')
    .click();
  cy.on('window:confirm', () => false);
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0)}/modify`
  );

  validateCT(getTableName(0), 'success');
};

export const passMTDeleteTable = () => {
  cy.get('button')
    .contains('Delete table')
    .click();
  cy.on('window:confirm', () => true);
  cy.wait(5000);
  cy.url().should('eq', `${baseUrl}/data/schema/public`);
  validateCT(getTableName(0), 'failure');
};

export const failMTRemoveNullable = () => {
  cy.get('a')
    .contains('Insert Row')
    .click();
  cy.get('input[placeholder = "integer"]').type('{selectall}{del}');
  cy.get('input[placeholder="integer"]').type(123);
  cy.get('button')
    .contains('Save')
    .click();
  cy.get('input[placeholder = "integer"]').type('{selectall}{del}');
  cy.get('input[placeholder="integer"]').type(1234);
  cy.get('button')
    .contains('Insert Again')
    .click();
  cy.get('a')
    .contains('Modify')
    .click();
  Addcolumnnullable();
};

export const setValidationMetaData = () => {
  setMetaData();
};

//  Views Modify /////////////////////////////////////////////////

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

export const Createtables = () => {
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
};

export const Createview = () => {
  cy.get('a')
    .contains('SQL')
    .click();
  cy.get('textarea').type(`CREATE VIEW author_average_rating AS
    SELECT author_table.id, avg(article_table.rating)
    From author_table, article_table
    WHERE author_table.id = article_table.author_id
    GROUP BY author_table.id`);
  cy.get('button')
    .contains('Run!')
    .click();
  validateCT('author_average_rating', 'success');
};

export const Checkviewtable = () => {
  cy.get('a')
    .contains('author_average_rating')
    .click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/views/author_average_rating/browse`
  );
  cy.get('a')
    .contains('Modify')
    .click();
  cy.get('button')
    .contains('Modify')
    .last()
    .click();
  cy.url().should('eq', `${baseUrl}/data/sql`);
};

export const Checkviewtabledelete = () => {
  cy.get('a')
    .contains('author_average_rating')
    .click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/views/author_average_rating/browse`
  );
  cy.get('a')
    .contains('Modify')
    .click();
  cy.get('button')
    .contains('Delete view')
    .last()
    .click();
  cy.on('window:confirm', str => {
    expect(str === 'Are you sure?').to.be.true;
    return true;
  });

  cy.wait(7000);
  validateCT('author_average_rating', 'failure');
};

export const Issue = () => {
  cy.get('.ace_text-input')
    .first()
    .type('#include');
};
