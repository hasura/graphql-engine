import {
  getElementFromAlias,
  getTableName,
  getTriggerName,
  getWebhookURL,
  getNoOfRetries,
  getIntervalSeconds,
  getTimeoutSeconds,
  baseUrl,
} from '../../../helpers/eventHelpers';
import {
  getColName,
  tableColumnTypeSelector,
} from '../../../helpers/dataHelpers';
import {
  setMetaData,
  validateCT,
  validateCTrigger,
  validateInsert,
  ResultType,
} from '../../validators/validators';
import { setPromptValue } from '../../../helpers/common';

const EVENT_TRIGGER_INDEX_ROUTE = '/events/data';

const testName = 'ctr'; // create trigger

export const passPTCreateTable = () => {
  // Click on create table
  cy.get(getElementFromAlias('data-create-table')).click();
  // Match the URL
  cy.url().should('eq', `${baseUrl}/data/default/schema/public/table/add`);
  // Type table name
  cy.get(getElementFromAlias('tableName')).type(getTableName(0, testName));
  // Set first column
  cy.get(getElementFromAlias('column-0')).type(getColName(0));
  tableColumnTypeSelector('col-type-0');
  cy.get(getElementFromAlias('data_test_column_type_value_serial'))
    .first()
    .click();
  // cy.get(getElementFromAlias('col-type-0')).select('serial');
  // Set second column
  cy.get(getElementFromAlias('column-1')).type(getColName(1));
  tableColumnTypeSelector('col-type-1');
  cy.get(getElementFromAlias('data_test_column_type_value_integer'))
    .first()
    .click();

  // cy.get(getElementFromAlias('col-type-1')).select('integer');
  // Set third column
  cy.get(getElementFromAlias('column-2')).type(getColName(2));
  tableColumnTypeSelector('col-type-2');
  cy.get(getElementFromAlias('data_test_column_type_value_text'))
    .first()
    .click();
  // cy.get(getElementFromAlias('col-type-2')).select('text');
  // Set primary key
  cy.get(getElementFromAlias('primary-key-select-0')).select('0');
  // Create
  cy.get(getElementFromAlias('table-create')).click();
  cy.wait(7000);
  cy.url().should(
    'eq',
    `${baseUrl}/data/default/schema/public/tables/${getTableName(
      0,
      testName
    )}/modify`
  );
};

export const checkCreateTriggerRoute = () => {
  //    Click on the create trigger button
  cy.visit(`${EVENT_TRIGGER_INDEX_ROUTE}/manage`);
  cy.wait(4000);
  cy.visit(EVENT_TRIGGER_INDEX_ROUTE);
  cy.wait(15000);
  cy.get(getElementFromAlias('data-sidebar-add')).click();
  //   Match the URL
  cy.url().should('eq', `${baseUrl}${EVENT_TRIGGER_INDEX_ROUTE}/add`);
};

export const failCTWithoutData = () => {
  //    Type trigger name
  cy.get(getElementFromAlias('trigger-name')).type(getTriggerName(0, testName));
  //    Click on create
  cy.get(getElementFromAlias('trigger-create')).click();
  //    Check if the route didn't change
  cy.url().should('eq', `${baseUrl}${EVENT_TRIGGER_INDEX_ROUTE}/add`);
  //   Validate
  validateCT(getTriggerName(0, testName), ResultType.FAILURE);
};

export const passCT = () => {
  // Set trigger name and select table
  cy.get(getElementFromAlias('trigger-name'))
    .clear()
    .type(getTriggerName(0, testName));
  cy.get(getElementFromAlias('select-source')).select('default');
  cy.wait(500);
  cy.get(getElementFromAlias('select-schema')).select('public');
  cy.get(getElementFromAlias('select-table')).select(getTableName(0, testName));

  // operations
  cy.get(getElementFromAlias('insert-operation')).check();
  cy.get(getElementFromAlias('update-operation')).check();
  cy.get(getElementFromAlias('delete-operation')).check();

  // webhook url
  cy.get(getElementFromAlias('webhook-input')).clear().type(getWebhookURL());

  // advanced settings
  cy.get(getElementFromAlias('advanced-settings')).click();

  // retry configuration
  cy.get(getElementFromAlias('no-of-retries')).clear().type(getNoOfRetries());
  cy.get(getElementFromAlias('interval-seconds'))
    .clear()
    .type(getIntervalSeconds());
  cy.get(getElementFromAlias('timeout-seconds'))
    .clear()
    .type(getTimeoutSeconds());

  //  Click on create
  cy.get(getElementFromAlias('trigger-create')).click();
  cy.wait(10000);
  //  Check if the trigger got created and navigated to processed events page
  cy.url().should(
    'eq',
    `${baseUrl}${EVENT_TRIGGER_INDEX_ROUTE}/${getTriggerName(
      0,
      testName
    )}/modify`
  );
  cy.get(getElementFromAlias(getTriggerName(0, testName)));
  //   Validate
  validateCTrigger(
    getTriggerName(0, testName),
    getTableName(0, testName),
    'public',
    ResultType.SUCCESS
  );
};

export const failCTDuplicateTrigger = () => {
  //  Visit create trigger page
  cy.visit(`${EVENT_TRIGGER_INDEX_ROUTE}/add`);
  // trigger and table name
  cy.get(getElementFromAlias('trigger-name'))
    .clear()
    .type(getTriggerName(0, testName));
  cy.get(getElementFromAlias('select-source')).select('default');
  cy.get(getElementFromAlias('select-table')).select(getTableName(0, testName));

  // operations
  cy.get(getElementFromAlias('insert-operation')).check();
  cy.get(getElementFromAlias('update-operation')).check();
  cy.get(getElementFromAlias('delete-operation')).check();

  // webhook url
  cy.get(getElementFromAlias('webhook-input')).clear().type(getWebhookURL());

  // FIXME: Commenting this for now. Uncomment once, the server issue is fixed.

  //  click on create
  // cy.get(getElementFromAlias('trigger-create')).click();
  // cy.wait(5000);
  //  should be on the same URL
  // cy.url().should('eq', `${baseUrl}${EVENT_TRIGGER_INDEX_ROUTE}/add`);
  cy.visit(`${baseUrl}${EVENT_TRIGGER_INDEX_ROUTE}/add`);
  cy.wait(4000);
};

export const insertTableRow = () => {
  // visit insert row page
  cy.visit(
    `/data/default/schema/public/tables/${getTableName(0, testName)}/insert`
  );
  // one serial column. so insert a row directly.
  cy.get(getElementFromAlias(`typed-input-${1}`)).type('123');
  cy.get(getElementFromAlias(`typed-input-${2}`)).type('Some text');
  cy.get(getElementFromAlias('insert-save-button')).click();
  cy.wait(300);
  validateInsert(getTableName(0, testName), 1);
  // now it should invoke the trigger to webhook
  cy.wait(10000);
  // check if processed events has a row and it is a successful response
  cy.visit(
    `${EVENT_TRIGGER_INDEX_ROUTE}/${getTriggerName(0, testName)}/processed`
  );
  cy.wait(10000);
  cy.get('.rt-tr-group').should('have.length.gte', 1);
};

export const deleteCTTestTrigger = () => {
  //  Go to the settings section of the trigger
  cy.visit(
    `${EVENT_TRIGGER_INDEX_ROUTE}/${getTriggerName(0, testName)}/processed`
  );
  //  click on settings tab
  cy.get(getElementFromAlias('trigger-modify')).click();
  setPromptValue(getTriggerName(0, testName));
  //  Click on delete
  cy.get(getElementFromAlias('delete-trigger')).click();
  //  Confirm
  cy.window().its('prompt').should('be.called');
  cy.wait(7000);
  //  Match the URL
  cy.url().should('eq', `${baseUrl}${EVENT_TRIGGER_INDEX_ROUTE}/manage`);
  //  Validate
  validateCTrigger(
    getTriggerName(0, testName),
    getTableName(0, testName),
    'public',
    ResultType.FAILURE
  );
};

export const deleteCTTestTable = () => {
  //   Go to the modify section of the table
  cy.visit(
    `/data/default/schema/public/tables/${getTableName(0, testName)}/browse`
  );
  cy.get(getElementFromAlias('table-modify')).click();
  //   Click on delete
  setPromptValue(getTableName(0, testName));
  cy.get(getElementFromAlias('delete-table')).click();
  //   Confirm
  cy.window().its('prompt').should('be.called');
  cy.wait(7000);
  //   Match the URL
  cy.url().should('eq', `${baseUrl}/data/default/schema/public`);
  //   Validate
  validateCT(getTableName(0, testName), ResultType.FAILURE);
};

export const setValidationMetaData = () => {
  setMetaData();
};
