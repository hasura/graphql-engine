import {
  baseUrl,
  getColName,
  getTableName,
  dataTypes,
  getElementFromAlias,
  typeDefaults,
} from '../../../helpers/dataHelpers';

import {
  validateInsert,
  setMetaData,
  validateCT,
} from '../../validators/validators';

const numOfDataTypes = dataTypes.length;

//* ******************** Util functions ************************

const setColumns = () => {
  // cy.scrollTo('right');
  for (let i = 0; i < numOfDataTypes; i += 1) {
    // Type column name
    cy.get(getElementFromAlias(`column-${i}`)).type(getColName(i));
    // Select column type
    cy.get(getElementFromAlias(`col-type-${i}`)).select(dataTypes[i]);

    if (i === 4) {
      cy.get(getElementFromAlias(`col-type-${i}`))
        .parent()
        .find('[type="checkbox"]')
        .last()
        .check();
      // cy.get('[type="checkbox"]').last().check()
    }
    // Set appropriate default if the type is not serial
    if (i > 1) {
      cy.get(getElementFromAlias(`col-default-${i}`)).type(
        typeDefaults[dataTypes[i]]
      );
    }
  }
};

const clickSaveOrInsert = (firstIndex, currentIndex) => {
  if (currentIndex === firstIndex) {
    cy.get('button')
      .contains('Save')
      .click();
  } else {
    cy.get('button')
      .contains('Insert Again')
      .click();
  }
  cy.wait(2000);
};

const checkQuerySuccess = () => {
  // Expect only 4 rows i.e. expect fifth element to not exist
  cy.get('[role=gridcell]').contains('filter-text');
  cy.get('[role=row]')
    .eq(2)
    .should('not.exist');
};

const checkOrder = order => {
  // Utility function to get right element

  const curElement = cy.get('[role=row]');
  if (order === 'asc') {
    curElement.each(($el, index) => {
      if (index !== 0) {
        cy.wrap($el)
          .find('[role=gridcell]')
          .first()
          .next()
          .contains(index);
      }
    });
  } else {
    curElement.each(($el, index) => {
      if (index !== 0) {
        cy.wrap($el)
          .find('[role=gridcell]')
          .first()
          .next()
          .contains(22 - index);
      }
    });
  }
};

//* ******************** Test functions ***************************

export const passBICreateTable = () => {
  cy.wait(7000);
  // Click create table button
  cy.get('button')
    .contains('Create Table')
    .click();
  // Type table name
  cy.get(getElementFromAlias('tableName')).type(getTableName(0));
  // Set columns with all fields
  setColumns();
  // Set primary key
  cy.get(getElementFromAlias('primary-key-select-0')).select('0');
  // Click on create
  cy.get(getElementFromAlias('table-create')).click();
  cy.wait(7000);
  validateCT(getTableName(0), 'success');
};

export const checkInsertRoute = () => {
  // Click on Insert tab
  cy.get('a')
    .contains(getTableName(0))
    .click();
  cy.get(getElementFromAlias('table-insert-rows')).click();
  // Match URL
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0)}/insert`
  );
};

export const failBIWrongDataType = () => {
  // Check if the table creation fails for wrong inputs of each data type
  for (let i = 2; i < numOfDataTypes; i += 1) {
    // Text and Boolean always succeed, so we check only for others
    if (dataTypes[i] !== 'text' && dataTypes[i] !== 'boolean') {
      const sureFailString = 'abcd1234';
      // Type a string that fails
      cy.get(getElementFromAlias(`typed-input-${i}`)).type(sureFailString);
      // Click the Save/Insert Again button.
      clickSaveOrInsert(2, i);
      // Check for error and dismiss it
      cy.get('[class=notification-title]')
        .contains('Insert failed')
        .click();
      // Check the default radio of current column
      cy.get(getElementFromAlias(`typed-input-default-${i}`)).check();
    }

    validateInsert(getTableName(0), 0);
  }
};

export const passBIInsert20Rows = () => {
  for (let i = 0; i < 20; i += 1) {
    // Type a string in the text type fields of some rows  (to be tested in Browse rows)
    const textIndex = dataTypes.indexOf('text');
    // Click the Insert Again button.
    if (i === 0) {
      cy.get(getElementFromAlias(`typed-input-${textIndex}`)).type(
        '{selectall}{del}'
      );
      cy.get(getElementFromAlias(`typed-input-${textIndex}`)).type(
        'filter-text'
      );
      cy.get('button')
        .contains('Insert Again')
        .click();
      continue; // eslint-disable-line
    }
    cy.get(getElementFromAlias(`typed-input-${textIndex}`)).type(
      '{selectall}{del}'
    );
    cy.get(getElementFromAlias(`typed-input-${textIndex}`))
      .type('{selectall}{del}')
      .type(
        Math.random()
          .toString(36)
          .substring(7)
      );
    cy.get('button')
      .contains('Insert Again')
      .click();
    validateInsert(getTableName(0), i + 1);
  }
  // Wait for insert notifications to disappear
  cy.wait(7000);
};

export const checkBrowseRoute = () => {
  // Click on Browse tab
  cy.get('a')
    .contains(getTableName(0))
    .click();
  cy.get(getElementFromAlias('table-browse-rows')).click();
  cy.wait(2000);
  // Match URL
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0)}/browse`
  );
};

export const passBI20RowsExist = () => {
  // Check if the 20 inserted elements reflect in the UI
  cy.get(getElementFromAlias('table-browse-rows')).contains('21');
  // Check pagination string
};

export const passBISort = order => {
  // Scroll to top TODO responsive is messy
  cy.wait(7000);
  // cy.scrollTo('top');
  // Select column with type 'serial'
  const serialIndex = dataTypes.indexOf('serial');
  cy.get(getElementFromAlias('sort-column-0')).select(getColName(serialIndex));
  // Select order as `descending`
  cy.get(getElementFromAlias('sort-order-0')).select(
    order === 'asc' ? 'Asc' : 'Desc'
  );
  // Run query
  cy.get('button')
    .contains('Run query')
    .click();
  cy.wait(5000);
  // Check order
  checkOrder(order);

  // Clear filter
  cy.get(getElementFromAlias('clear-sorts-0')).click();
  // Run query
  cy.get('button')
    .contains('Run query')
    .click();
  cy.wait(5000);
};

export const passBIFilterQueryEq = () => {
  // Select column with type "text"
  const textIndex = dataTypes.indexOf('text');
  cy.get(getElementFromAlias('filter-column-0')).select(getColName(textIndex));
  // Select operator as `eq`
  cy.get(getElementFromAlias('filter-op-0')).select('$eq');
  // Type value as "filter-text"
  cy.get("input[placeholder='-- value --']")
    .last()
    .type('filter-text');
  // Run query
  cy.get('button')
    .contains('Run query')
    .click();
  cy.wait(2000);
  // Check if the query was successful
  checkQuerySuccess();

  // Clear filter
  cy.get(getElementFromAlias('clear-filter-0')).click();
  // Run query
  cy.get('button')
    .contains('Run query')
    .click();
  cy.wait(5000);
};

export const deleteBITestTable = () => {
  // Go to the modify section of the table
  cy.get(getElementFromAlias('table-modify')).click();
  cy.wait(2000);
  // Click on delete
  cy.get('button')
    .contains('Delete table')
    .click();
  // Confirm
  cy.on('window:confirm', str => {
    expect(str === 'Are you sure?').to.be.true;
    return true;
  });
  cy.wait(7000);
  // Match the URL
  cy.url().should('eq', `${baseUrl}/data/schema/public`);
  validateCT(getTableName(0), 'failure');
};

export const failBINullKeys = () => {
  const textIndex = dataTypes.indexOf('text');

  cy.get(getElementFromAlias(`typed-input-${textIndex}`))
    .clear()
    .type('filter-text');

  cy.get("input[placeholder='float']")
    .first()
    .type('{selectall}{del}');

  // Click the Insert Again button.
  cy.get('button')
    .contains('Insert Again')
    .click();

  cy.get('[class=notification-title]')
    .contains('Insert failed')
    .click();
  // Wait for insert notifications to disappear
  cy.wait(7000);
  validateInsert(getTableName(0), 20);
};

export const failBIUniqueKeys = () => {
  // Type a string in the text type fields of some rows  (to be tested in Browse rows)
  const textIndex = dataTypes.indexOf('text');
  cy.get("input[placeholder='float']")
    .first()
    .type(0.5555);

  cy.get(getElementFromAlias(`typed-input-${textIndex}`))
    .clear()
    .type('filter-text');

  // Click the Insert Again button.
  cy.get('input[placeholder="text"]')
    .first()
    .type('{selectall}{del}');
  cy.get('input[placeholder="text"]')
    .first()
    .type('name');
  cy.get('button')
    .contains('Insert Again')
    .click();
  // Check default for next insert

  cy.get(getElementFromAlias(`typed-input-default-${textIndex}`)).check();

  validateInsert(getTableName(0), 21);
  cy.wait(7000);
  cy.get(getElementFromAlias(`typed-input-${textIndex}`))
    .clear()
    .type('filter-text');
  // Click the Insert Again button.
  cy.get('input[placeholder="text"]')
    .first()
    .type('{selectall}{del}');
  cy.get('input[placeholder="text"]')
    .first()
    .type('name');
  cy.get('button')
    .contains('Insert Again')
    .click();

  cy.get('[class=notification-title]')
    .contains('Insert failed')
    .click();
  cy.wait(7000);
  validateInsert(getTableName(0), 21);
};
export const setValidationMetaData = () => {
  setMetaData();
};

// /////////////////////////////////////    Buttons /////////////////////////////////
export const passEditButton = () => {
  cy.get(getElementFromAlias('table-browse-rows')).click();
  cy.wait(2000);
  cy.get(getElementFromAlias('row-edit-button-0')).click();
  cy.wait(2000);
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0)}/edit`
  );
  const textIndex = dataTypes.indexOf('text');
  cy.get(getElementFromAlias(`typed-input-${textIndex}`)).type(
    '{selectall}{del}'
  );
  cy.get(getElementFromAlias(`typed-input-${textIndex}`)).type('new-text');
  cy.get('button')
    .contains('Save')
    .last()
    .click();
  cy.get('h4').contains('Edited!', { timeout: 7000 });
};

export const passCloneButton = () => {
  cy.get(getElementFromAlias('table-browse-rows')).click();
  cy.wait(5000);
  cy.get(getElementFromAlias('row-clone-button-0')).click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0)}/insert`
  );
};

export const passDeleteRow = () => {
  cy.get(getElementFromAlias('table-browse-rows')).click();
  cy.wait(5000);
  cy.get(getElementFromAlias('row-delete-button-0')).click();
  cy.on('window:confirm', str => {
    expect(str === 'Permanently delete this row?').to.be.true;
  });
  cy.get('h4').contains('Row deleted!', { timeout: 7000 });
  cy.wait(7000);
};
