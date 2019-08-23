import {
  baseUrl,
  getColName,
  getTableName,
  dataTypes,
  getElementFromAlias,
  typeDefaults,
  tableColumnTypeSelector,
} from '../../../helpers/dataHelpers';

import {
  validateInsert,
  setMetaData,
  validateCT,
} from '../../validators/validators';

const numOfDataTypes = dataTypes.length;
const testName = 'ib';

//* ******************** Util functions ************************

const setColumns = () => {
  // cy.scrollTo('right');
  for (let i = 0; i < numOfDataTypes; i += 1) {
    // Type column name
    cy.get(getElementFromAlias(`column-${i}`)).type(getColName(i));
    // Select column type
    tableColumnTypeSelector(`col-type-${i}`);
    // cy.get(getElementFromAlias(`col-type-${i}`)).click();
    cy.get(getElementFromAlias(`data_test_column_type_value_${dataTypes[i]}`))
      .first()
      .click();
    // cy.get(getElementFromAlias(`col-type-${i}`)).select(dataTypes[i]);

    if (i === dataTypes.indexOf('text')) {
      cy.get(getElementFromAlias(`unique-${i}`)).check();
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
    cy.get(getElementFromAlias('insert-save-button')).click();
  } else {
    cy.get(getElementFromAlias('insert-save-button')).click();
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
  cy.get(getElementFromAlias('data-create-table')).click();
  // Type table name
  cy.get(getElementFromAlias('tableName')).type(getTableName(0, testName));
  // Set columns with all fields
  setColumns();
  // Set primary key
  cy.get(getElementFromAlias('primary-key-select-0')).select('0');
  // Click on create
  cy.get(getElementFromAlias('table-create')).click();
  cy.wait(7000);
  validateCT(getTableName(0, testName), 'success');
};

export const passSearchTables = () => {
  // Click add table button
  cy.get(getElementFromAlias('sidebar-add-table')).click();
  // Type table name
  cy.get(getElementFromAlias('tableName')).type(getTableName(1, testName));
  // Type column name
  cy.get(getElementFromAlias('column-0')).type(getColName(0));
  // Select column type
  // cy.get(getElementFromAlias('col-type-0')).select('integer');
  tableColumnTypeSelector('col-type-0');
  cy.get(getElementFromAlias('data_test_column_type_value_integer'))
    .first()
    .click();
  // Set primary key
  cy.get(getElementFromAlias('primary-key-select-0')).select('0');
  // Click on create
  cy.get(getElementFromAlias('table-create')).click();
  cy.wait(7000);
  validateCT(getTableName(0, testName), 'success');
  cy.get(getElementFromAlias('search-tables')).type('0');
  cy.get(getElementFromAlias('table-links')).should('not.contain', '1');
  cy.get(getElementFromAlias('search-tables')).type('{home}{del}');
};

export const checkInsertRoute = () => {
  // Click on Insert tab
  cy.get(getElementFromAlias(getTableName(0, testName))).click();
  cy.get(getElementFromAlias('table-insert-rows')).click();
  // Match URL
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0, testName)}/insert`
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
      cy.get(getElementFromAlias(`typed-input-${i}`)).clear();
      // Check for error and dismiss it
      // cy.get('[class=notification-title]')
      //   .contains('Insert failed')
      //   .click();
      // cy.get('.notification-error').click();
      // Check the default radio of curret column
      cy.get(getElementFromAlias(`typed-input-default-${i}`)).check();
    }

    validateInsert(getTableName(0, testName), 0);
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
      cy.get(getElementFromAlias('insert-save-button')).click();
    } else {
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
      cy.get(
        getElementFromAlias(`typed-input-default-${textIndex + 1}`)
      ).check();
      cy.get(getElementFromAlias('insert-save-button')).click();
      cy.wait(300);
      validateInsert(getTableName(0, testName), i + 1);
    }
  }
  // Wait for insert notifications to disappear
  cy.wait(7000);
};

export const checkBrowseRoute = () => {
  // Click on Browse tab
  cy.get(getElementFromAlias(getTableName(0, testName))).click();
  cy.get(getElementFromAlias('table-browse-rows')).click();
  cy.wait(2000);
  // Match URL
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0, testName)}/browse`
  );
};

export const passBI20RowsExist = () => {
  // Check if the 20 inserted elements reflect in the UI
  cy.get(getElementFromAlias('table-browse-rows')).contains('21');
};

export const checkPagination = () => {
  // Check if the current page is 1
  cy.get('.-pageJump > input').should('have.value', '1');
  // Check if the total number of pages is 3
  cy.get('.-totalPages').contains('3');
  // Check if the default value of rows displayed is 10
  cy.get('.-pageSizeOptions > select').should('have.value', '10');
  cy.get('.-next > button').click();
  cy.wait(3000);
  // Check if the page changed
  cy.get(
    '.rt-tbody > div:nth-child(1) > div > div:nth-child(2) > div'
  ).contains('11');
  cy.get('.-pageJump > input').should('have.value', '2');
  cy.get('.-previous > button').click();
  cy.wait(3000);
  // Check if the page changed
  cy.get('.-pageJump > input').should('have.value', '1');
  cy.get('.-pageSizeOptions > select').select('5 rows');
  cy.wait(3000);
  // Check if the total number of pages changed
  cy.get('.-totalPages').contains('5');
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
  cy.get(getElementFromAlias('run-query')).click();
  cy.wait(5000);
  // Check order
  checkOrder(order);

  // Clear filter
  cy.get(getElementFromAlias('clear-sorts-0')).click();
  // Run query
  cy.get(getElementFromAlias('run-query')).click();
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
  cy.get(getElementFromAlias('run-query')).click();
  cy.wait(2000);
  // Check if the query was successful
  checkQuerySuccess();

  // Clear filter
  cy.get(getElementFromAlias('clear-filter-0')).click();
  // Run query
  cy.get(getElementFromAlias('run-query')).click();
  cy.wait(5000);
};

export const deleteBITestTable = () => {
  cy.get(getElementFromAlias(getTableName(2, testName))).click();
  // Go to the modify section of the table
  cy.get(getElementFromAlias('table-modify')).click();
  cy.wait(2000);
  // Click on delete
  cy.get(getElementFromAlias('delete-table')).click();
  // Confirm
  cy.on('window:confirm', str => {
    expect(str === 'Are you sure?').to.be.true;
    return true;
  });
  cy.wait(7000);
  // Match the URL
  cy.url().should('eq', `${baseUrl}/data/schema/public`);
  validateCT(getTableName(2, testName), 'failure');
  cy.get(getElementFromAlias(getTableName(1, testName))).click();
  // Go to the modify section of the table
  cy.get(getElementFromAlias('table-modify')).click();
  cy.wait(2000);
  // Click on delete
  cy.get(getElementFromAlias('delete-table')).click();
  // Confirm
  cy.on('window:confirm', str => {
    expect(str === 'Are you sure?').to.be.true;
    return true;
  });
  cy.wait(7000);
  // Match the URL
  cy.url().should('eq', `${baseUrl}/data/schema/public`);
  validateCT(getTableName(1, testName), 'failure');
  cy.get(getElementFromAlias(getTableName(0, testName))).click();
  // Go to the modify section of the table
  cy.get(getElementFromAlias('table-modify')).click();
  cy.wait(2000);
  // Click on delete
  cy.get(getElementFromAlias('delete-table')).click();
  // Confirm
  cy.on('window:confirm', str => {
    expect(str === 'Are you sure?').to.be.true;
    return true;
  });
  cy.wait(7000);
  // Match the URL
  cy.url().should('eq', `${baseUrl}/data/schema/public`);
  validateCT(getTableName(0, testName), 'failure');
};

// export const failBINullKeys = () => {
//   const textIndex = dataTypes.indexOf('text');

//   cy.get(getElementFromAlias(`typed-input-${textIndex}`))
//     .clear()
//     .type('null-key-test');

//   cy.get("input[placeholder='float']")
//     .first()
//     .type('{selectall}{del}');

//   // Click the Insert Again button.
//   cy.get(getElementFromAlias('insert-save-button')).click();

//   // cy.get('.notification-error').click();
//   // Wait for insert notifications to disappear
//   cy.wait(7000);
//   validateInsert(getTableName(0, testName), 20);
// };

export const failBIUniqueKeys = () => {
  // Type a string in the text type fields of some rows  (to be tested in Browse rows)
  const textIndex = dataTypes.indexOf('text');
  const floatIndex = dataTypes.indexOf('numeric');
  cy.get(getElementFromAlias(`typed-input-${floatIndex}`)).type(0.5555);
  cy.get(getElementFromAlias(`typed-input-${textIndex}`))
    .clear()
    .type('filter-text');

  // Click the Insert Again button.
  cy.get(getElementFromAlias(`typed-input-${textIndex}`)).type(
    '{selectall}{del}'
  );
  cy.get(getElementFromAlias(`typed-input-${textIndex}`)).type('name');

  cy.get(getElementFromAlias('insert-save-button')).click();
  // Check default for next insert

  cy.get(getElementFromAlias(`typed-input-default-${textIndex}`)).check();

  validateInsert(getTableName(0, testName), 21);
  cy.wait(7000);
  cy.get(getElementFromAlias(`typed-input-${textIndex}`))
    .clear()
    .type('filter-text');
  // Click the Insert Again button.
  cy.get(getElementFromAlias(`typed-input-${textIndex}`)).type(
    '{selectall}{del}'
  );
  cy.get(getElementFromAlias(`typed-input-${textIndex}`)).type('name');
  cy.get(getElementFromAlias('insert-save-button')).click();

  // cy.get('.notification-error').click();
  cy.wait(7000);
  validateInsert(getTableName(0, testName), 21);
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
    `${baseUrl}/data/schema/public/tables/${getTableName(0, testName)}/edit`
  );
  const textIndex = dataTypes.indexOf('text');
  cy.get(getElementFromAlias(`typed-input-${textIndex}`)).type(
    '{selectall}{del}'
  );
  cy.get(getElementFromAlias(`typed-input-${textIndex}`)).type('new-text');
  cy.get(getElementFromAlias('save-button')).click();
  // cy.get('h4').contains('Edited!', { timeout: 7000 });
  // cy.get('.notification-error');
  cy.wait(7000);
};

export const passCloneButton = () => {
  cy.get(getElementFromAlias('table-browse-rows')).click();
  cy.wait(5000);
  cy.get(getElementFromAlias('row-clone-button-0')).click();
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0, testName)}/insert`
  );
  cy.get(getElementFromAlias('clear-button')).click();
  cy.get(getElementFromAlias('typed-input-0')).should('have.value', '');
};

export const checkViewRelationship = () => {
  cy.get(getElementFromAlias('sidebar-add-table')).click();
  // Type table name
  cy.get(getElementFromAlias('tableName')).type(getTableName(2, testName));
  cy.get(getElementFromAlias('column-0')).type('id');
  tableColumnTypeSelector('col-type-0');
  cy.get(getElementFromAlias('data_test_column_type_value_serial'))
    .first()
    .click();
  cy.get(getElementFromAlias('column-1')).type('someID');
  tableColumnTypeSelector('col-type-1');
  cy.get(getElementFromAlias('data_test_column_type_value_integer'))
    .first()
    .click();
  // Set primary key
  cy.get(getElementFromAlias('primary-key-select-0')).select('0');
  // Click on create
  cy.get(getElementFromAlias('table-create')).click();
  cy.wait(7000);
  validateCT(getTableName(0, testName), 'success');
  // Add foreign key
  cy.get(getElementFromAlias('modify-table-edit-fk-0')).click();
  cy.get(getElementFromAlias('foreign-key-ref-table-0')).select(
    getTableName(0, testName)
  );
  cy.get(getElementFromAlias('foreign-key-0-lcol-0')).select('0');
  cy.get(getElementFromAlias('foreign-key-0-rcol-0')).select(getColName(0));
  cy.get(getElementFromAlias('modify-table-fk-0-save')).click();
  cy.wait(5000);
  // Add relationship
  cy.get(getElementFromAlias('table-relationships')).click();
  cy.get(getElementFromAlias('obj-rel-add-0')).click();
  cy.get(getElementFromAlias('suggested-rel-name'))
    .clear()
    .type('someRel');
  cy.get(getElementFromAlias('obj-rel-save-0')).click();
  cy.wait(2000);
  // Insert a row
  cy.get(getElementFromAlias('table-insert-rows')).click();
  cy.get(getElementFromAlias('typed-input-1')).type('1');
  cy.get(getElementFromAlias('insert-save-button')).click();
  cy.wait(1000);
  cy.get(getElementFromAlias('table-browse-rows')).click();
  cy.wait(1000);
  cy.get('a')
    .contains('View')
    .first()
    .click();
  cy.wait(1000);
  cy.get('a')
    .contains('Close')
    .first()
    .click();
};

export const passDeleteRow = () => {
  cy.get(getElementFromAlias('table-browse-rows')).click();
  cy.wait(5000);
  cy.get(getElementFromAlias('row-delete-button-0')).click();
  cy.on('window:confirm', str => {
    expect(str === 'Permanently delete this row?').to.be.true;
  });
  // cy.get('.notification-error');
  cy.wait(14000);
};
