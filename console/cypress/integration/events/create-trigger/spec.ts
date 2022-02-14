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
  validateInsert,
  ResultType,
  validateCTrigger,
} from '../../validators/validators';
import { setPromptValue } from '../../../helpers/common';
import {
  toggleRequestTransformSection,
  togglePayloadTransformSection,
  typeIntoRequestQueryParams,
  typeIntoRequestUrl,
  typeIntoTransformBody,
  checkTransformRequestUrlError,
  checkTransformRequestBodyError,
  checkTransformRequestUrlPreview,
  clearPayloadTransformBody,
  clearRequestUrl,
} from '../../../helpers/webhookTransformHelpers';
import { AWAIT_LONG, AWAIT_SHORT } from '../../../helpers/constants';

const EVENT_REQUEST_BODY_TRANSFORM_TEXTAREA = 1;

const EVENT_TRIGGER_INDEX_ROUTE = '/events/data';

const testName = 'ctr'; // create trigger

const statements = {
  createTransformIncorrectPayloadBody: `
  {
    "tableInfo": {
      "name": {{$table.name}}
  `,
  createTransformPayloadBody: `
  {
    "tableInfo": {
      "name": {{$body.table.name}},
      "schema": {{$body.table.schema}},
      "trigger": {{$body.trigger.name}}
  `,
};

const createETForm = (allCols: boolean) => {
  // Set trigger name and select table
  cy.getBySel('trigger-name').clear().type(getTriggerName(0, testName));
  cy.getBySel('select-source').select('default');
  cy.wait(500);
  cy.getBySel('select-schema').select('public');
  cy.getBySel('select-table').select(getTableName(0, testName));

  // operations
  cy.getBySel('insert-operation').check();
  cy.getBySel('update-operation').check();
  cy.getBySel('delete-operation').check();

  // webhook url
  cy.getBySel('webhook-input').clear().type(getWebhookURL());

  // advanced settings
  cy.getBySel('advanced-settings').click();
  if (!allCols) {
    cy.getBySel('choose-column').click();
    cy.getBySel('select-column').first().click();
  }

  // retry configuration
  cy.getBySel('no-of-retries').clear().type(getNoOfRetries());
  cy.getBySel('interval-seconds').clear().type(getIntervalSeconds());
  cy.getBySel('timeout-seconds').clear().type(getTimeoutSeconds());
};

export const passPTCreateTable = () => {
  // Click on create table
  cy.getBySel('data-create-table').click();
  // Match the URL
  cy.url().should('eq', `${baseUrl}/data/default/schema/public/table/add`);
  // Type table name
  cy.getBySel('tableName').type(getTableName(0, testName));
  // Set first column
  cy.getBySel('column-0').type(getColName(0));
  tableColumnTypeSelector('col-type-0');
  cy.getBySel('data_test_column_type_value_serial').first().click();
  // cy.getBySel('col-type-0')).select('serial');
  // Set second column
  cy.getBySel('column-1').type(getColName(1));
  tableColumnTypeSelector('col-type-1');
  cy.getBySel('data_test_column_type_value_integer').first().click();

  // cy.getBySel('col-type-1')).select('integer');
  // Set third column
  cy.getBySel('column-2').type(getColName(2));
  tableColumnTypeSelector('col-type-2');
  cy.getBySel('data_test_column_type_value_text').first().click();
  // cy.getBySel('col-type-2')).select('text');
  // Set primary key
  cy.getBySel('primary-key-select-0').select('0');
  // Create
  cy.getBySel('table-create').click();
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
  cy.getBySel('data-sidebar-add').click();
  //   Match the URL
  cy.url().should('eq', `${baseUrl}${EVENT_TRIGGER_INDEX_ROUTE}/add`);
};

export const failCTWithoutData = () => {
  //    Type trigger name
  cy.getBySel('trigger-name').type(getTriggerName(0, testName));
  //    Click on create
  cy.getBySel('trigger-create').click();
  //    Check if the route didn't change
  cy.url().should('eq', `${baseUrl}${EVENT_TRIGGER_INDEX_ROUTE}/add`);
  //   Validate
  validateCT(getTriggerName(0, testName), ResultType.FAILURE);
};

export const passCT1 = () => {
  // select choose column from the radio input
  const allCols = false;
  createETForm(allCols);

  //  Click on create
  cy.getBySel('trigger-create').click();
  cy.wait(10000);
  //  Check if the trigger got created and navigated to processed events page
  cy.url().should(
    'eq',
    `${baseUrl}${EVENT_TRIGGER_INDEX_ROUTE}/${getTriggerName(
      0,
      testName
    )}/modify`
  );
  cy.getBySel(getTriggerName(0, testName));
  //   Validate
  validateCTrigger(
    getTriggerName(0, testName),
    getTableName(0, testName),
    'public',
    ResultType.SUCCESS,
    allCols
  );
};

export const passCT2 = () => {
  cy.getBySel('data-sidebar-add').click();
  // select all columns from the radio input
  const allCols = true;
  createETForm(allCols);

  //  Click on create
  cy.getBySel('trigger-create').click();
  cy.wait(10000);
  //  Check if the trigger got created and navigated to processed events page
  cy.url().should(
    'eq',
    `${baseUrl}${EVENT_TRIGGER_INDEX_ROUTE}/${getTriggerName(
      0,
      testName
    )}/modify`
  );
  cy.getBySel(getTriggerName(0, testName));
  //   Validate
  validateCTrigger(
    getTriggerName(0, testName),
    getTableName(0, testName),
    'public',
    ResultType.SUCCESS,
    allCols
  );
};

export const failCTDuplicateTrigger = () => {
  //  Visit create trigger page
  cy.visit(`${EVENT_TRIGGER_INDEX_ROUTE}/add`);
  // trigger and table name
  cy.getBySel('trigger-name').clear().type(getTriggerName(0, testName));
  cy.getBySel('select-source').select('default');
  cy.getBySel('select-schema').select('public');
  cy.getBySel('select-table').select(getTableName(0, testName));

  // operations
  cy.getBySel('insert-operation').check();
  cy.getBySel('update-operation').check();
  cy.getBySel('delete-operation').check();

  // webhook url
  cy.getBySel('webhook-input').clear().type(getWebhookURL());

  // FIXME: Commenting this for now. Uncomment once, the server issue is fixed.

  //  click on create
  // cy.getBySel('trigger-create')).click();
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
  cy.getBySel(`typed-input-${1}`).type('123');
  cy.getBySel(`typed-input-${2}`).type('Some text');
  cy.getBySel('insert-save-button').click();
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
  cy.getBySel('trigger-modify').click();
  setPromptValue(getTriggerName(0, testName));
  //  Click on delete
  cy.getBySel('delete-trigger').click();
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
  cy.get(getElementFromAlias('table-modify'), { timeout: 5000 }).click();
  //   Click on delete
  setPromptValue(getTableName(0, testName));
  cy.getBySel('delete-table').click();
  //   Confirm
  cy.window().its('prompt').should('be.called');
  cy.get(getElementFromAlias('data-create-table'), { timeout: 5000 }).should(
    'exist'
  );

  //   Match the URL
  cy.url().should('eq', `${baseUrl}/data/default/schema/public`);
  //   Validate
  validateCT(getTableName(0, testName), ResultType.FAILURE);
};

export const setValidationMetaData = () => {
  setMetaData();
};

export const clearHandler = () => {
  cy.getBySel('webhook-input').clear();
};

export const createEtTransform = () => {
  // open create event trigger form
  cy.getBySel('data-sidebar-add').click();
  //   Match the URL
  cy.url().should('eq', `${baseUrl}${EVENT_TRIGGER_INDEX_ROUTE}/add`);

  // fill up the basic event trigger form
  createETForm(true);

  // open request transform section
  toggleRequestTransformSection();
  cy.wait(AWAIT_SHORT);
  cy.getBySel('transform-POST').click();

  // give correct body without webhook handler
  clearHandler();
  typeIntoRequestUrl('{{$base_url}}');
  cy.wait(AWAIT_SHORT);
  // check for error
  checkTransformRequestUrlError(
    true,
    'Please configure your webhook handler to generate request url transform'
  );

  // clear handler
  clearHandler();
  // type into handler
  cy.getBySel('webhook-input').type(getWebhookURL());

  // give incorrect body
  clearRequestUrl();
  typeIntoRequestUrl('{{$url}}/users');
  cy.wait(AWAIT_SHORT);
  // check for error
  checkTransformRequestUrlError(true);

  // give correct body
  clearRequestUrl();
  typeIntoRequestUrl('/{{$body.trigger.name}}');
  typeIntoRequestQueryParams([
    { key: 'id', value: '5' },
    { key: 'tableName', value: '{{$body.table.name}}' },
  ]);
  cy.wait(AWAIT_SHORT);
  // check there is no error
  checkTransformRequestUrlError(false);
  // check the preview is correctly shown
  checkTransformRequestUrlPreview(
    'http://httpbin.org/post/Apic_test_trigger_ctr_0?id=5&tableName=Apic_test_table_ctr_0'
  );

  // open payload transform section
  togglePayloadTransformSection();
  // give incorrect body
  clearPayloadTransformBody(EVENT_REQUEST_BODY_TRANSFORM_TEXTAREA);
  typeIntoTransformBody(
    statements.createTransformIncorrectPayloadBody,
    EVENT_REQUEST_BODY_TRANSFORM_TEXTAREA
  );
  cy.wait(AWAIT_SHORT);
  checkTransformRequestBodyError(true);

  // give correct body
  clearPayloadTransformBody(EVENT_REQUEST_BODY_TRANSFORM_TEXTAREA);
  typeIntoTransformBody(
    statements.createTransformPayloadBody,
    EVENT_REQUEST_BODY_TRANSFORM_TEXTAREA
  );
  cy.wait(AWAIT_SHORT);
  checkTransformRequestBodyError(false);

  //  Click on create
  cy.getBySel('trigger-create').click();
  //  Check if the trigger got created
  cy.wait(AWAIT_SHORT);
  cy.get('.notification', { timeout: AWAIT_LONG })
    .should('be.visible')
    .and('contain', 'Event Trigger Created');
};

export const modifyEtTransform = () => {
  cy.getBySel('transform-GET').click();
  cy.getBySel('transform-requestUrl')
    .clear()
    .type('/{{$body.trigger.name}}', { parseSpecialCharSequences: false });
  cy.getBySel('transform-query-params-kv-remove-button-0').click();
  togglePayloadTransformSection();
  cy.getBySel('save-modify-trigger-changes').click();
  cy.wait(AWAIT_SHORT);
  cy.get('.notification', { timeout: AWAIT_LONG })
    .should('be.visible')
    .and('contain', 'Saved');
};

export const deleteEtTransform = () => {
  setPromptValue(getTriggerName(0, testName));
  //  Click on delete
  cy.getBySel('delete-trigger').click();
  //  Confirm
  cy.window().its('prompt').should('be.called');
  cy.wait(AWAIT_SHORT);
  cy.get('.notification', { timeout: AWAIT_LONG })
    .should('be.visible')
    .and('contain', 'Deleted event trigger');
};
