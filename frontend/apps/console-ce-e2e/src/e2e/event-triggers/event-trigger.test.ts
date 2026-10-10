import { HasuraMetadataV3, SchemaTable } from '@hasura/shared/types';
import { cliUrl, hgeUrl } from '../../support/endpoints';
import { readMetadata } from '../actions/withTransform/utils/services/readMetadata';
import { postgres } from '../data/manage-database/postgres.spec';

const EVENT_TRIGGER_NAME = 'event_trigger_test';

// Removes `event_trigger_test` whatever state a previous run left it in: from
// metadata, and its `notify_hasura_*` SQL functions, which HGE leaves behind
// (and then reports as "already exists") when the table is dropped via SQL
// while the trigger still exists.
const deleteLeftoverEventTrigger = () => {
  cy.request({
    method: 'POST',
    url: hgeUrl('/v1/metadata'),
    failOnStatusCode: false,
    body: {
      type: 'pg_delete_event_trigger',
      args: { name: EVENT_TRIGGER_NAME, source: 'default' },
    },
  });
  cy.request('POST', hgeUrl('/v2/query'), {
    type: 'run_sql',
    args: {
      source: 'default',
      sql: ['INSERT', 'UPDATE', 'DELETE']
        .map(
          (op) =>
            `DROP FUNCTION IF EXISTS hdb_catalog."notify_hasura_${EVENT_TRIGGER_NAME}_${op}"() CASCADE;`,
        )
        .join(' '),
    },
  });
};

describe('Create event trigger with shortest possible path', () => {
  before(() => {
    // If an earlier suite's `after` cleanup was interrupted it can leave both
    // `user_table` and its event trigger behind. Remove the trigger first (so
    // the non-cascading table drop is not blocked by a dependency), then drop
    // the leftover table, so this setup is self-contained and `createTable`
    // cannot fail with "relation already exists".
    deleteLeftoverEventTrigger();
    postgres.helpers.dropTableIfExists('user_table');

    // create a table first
    postgres.helpers.createTable('user_table');

    // Track the table as a prerequisite via the metadata API (see
    // postgres.helpers.trackTable) — deterministic setup. The event-trigger CRUD
    // under test is still driven through the UI in each `it`; only this setup
    // step moved off the UI, so these before-hooks no longer exercise the
    // Data-manager "untracked"/Track-button flow.
    postgres.helpers.trackTable('user_table');
  });
  // Runs before every attempt (including CI retries): a trigger left over
  // from a failed attempt would make the create fail with "already exists".
  beforeEach(() => {
    deleteLeftoverEventTrigger();
  });
  after(() => {
    // Delete the trigger before the table so a failed run doesn't leave its
    // SQL functions behind.
    deleteLeftoverEventTrigger();

    // delete the table
    cy.log('**--- Delete the table');
    postgres.helpers.deleteTable('user_table');
  });

  it('When the users create, modify and delete an Event trigger, everything should work', () => {
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**--- Step 1: Create an ET with shortest path**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');

    cy.visit('/events/data/manage', {
      timeout: 10000,
      onBeforeLoad(win) {
        cy.stub(win, 'prompt').returns('event_trigger_test');
      },
    });

    // --------------------
    cy.log('**--- Click on the Create button of the ET panel**');
    cy.get('[data-testid=data-create-trigger]').click();

    // trigger name
    cy.log('**--- Type the event trigger name**');
    cy.findByPlaceholderText('trigger_name').type('event_trigger_test');

    // select source
    cy.log('**--- Select the DB');
    cy.get('[name="source"]').radixSelect('default');

    // select schema and table (a single "schema / table" select)
    cy.log('**--- Select the schema and table');
    cy.get('[name="tableName"]').radixSelect('public / user_table');

    // select the trigger operation
    cy.log('**--- Select the ET operation');
    cy.findByRole('checkbox', { name: 'Insert' }).click();

    // add webhook url
    cy.log('**--- Add webhook url');
    cy.get('[name=handler]').type('http://httpbin.org/post');

    // click on create button to save ET
    cy.log('**--- Click on Create Event Trigger');
    cy.findByRole('button', { name: 'Create Event Trigger' }).click();

    // On success the console navigates to the trigger's modify page.
    cy.location('pathname', { timeout: 15000 }).should('include', '/modify');

    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**--- Step 2: Modify an ET to longest path and save it**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');

    // modfy the trigger operation
    cy.log(
      '**--- Click on Edit trigger operation and modfiy the trigger operation',
    );
    cy.get('[data-test=edit-operations]').click();
    cy.findByRole('checkbox', { name: 'Update' }).click();
    cy.findByRole('radio', { name: 'Choose columns' }).click();
    cy.findByRole('checkbox', { name: 'id' }).click();
    cy.findByRole('button', { name: 'Save' }).click();

    // modify the retry config
    cy.log('**--- Click on Edit retry config and modify the config');
    cy.get('[data-test=edit-retry-config]').click();
    cy.get('[name=num_retries]').clear().type('10');
    cy.get('[name=interval_sec]').clear().type('5');
    cy.get('[name=timeout_sec]').clear().type('70');
    cy.findByRole('button', { name: 'Save' }).click();

    // add headers
    cy.log('**--- Click on Edit retry config and add header');
    cy.get('[data-test=edit-header]').click();
    cy.findByPlaceholderText('key').type('x-hasura-user-id');
    cy.findByPlaceholderText('value').type('1234');
    cy.findByRole('button', { name: 'Save' }).click();

    // add Sample context
    cy.log('**--- Click on show sample context and fill the form');
    cy.findByText('Show Sample Context').click();
    cy.findByPlaceholderText('Key...').type('env-var');
    cy.findByPlaceholderText('Value...').type('env-var-value');

    // add Request Options Transform
    cy.log('**--- Click on Add Request Options Transform and fill the form');
    cy.findByText('Add Request Options Transform').click();
    cy.findByRole('radio', { name: 'GET' }).click();
    cy.get('[name=request_url]').type('/transformUrl');
    // The request URL is propagated into the saved form state on a 1s debounce
    // (editorDebounceTime); saving before it fires drops `request_transform.url`
    // from the metadata. The read-only Preview field is rendered from the
    // propagated value (with `{{$base_url}}` resolved to the webhook), so wait
    // for it to show the final URL before continuing — a deterministic wait on
    // the real state, not a fixed delay.
    cy.get('[data-test=transform-requestUrl-preview]').should(
      'have.value',
      'http://httpbin.org/post/transformUrl',
    );
    cy.findAllByPlaceholderText('Key...').eq(2).type('x-hasura-user-id');
    cy.findAllByPlaceholderText('Value...').eq(2).type('my-user-id');

    // add Payload Transform
    cy.log('**--- Click on Add Payload Transform and fill the form');
    cy.findByText('Add Payload Transform').click();

    // save the ET
    cy.log('**--- Click on Save Event trigger to modify the ET');
    cy.findByRole('button', { name: 'Save Event Trigger' }).click();

    readMetadata().then((md: { body: HasuraMetadataV3 }) => {
      cy.wrap(
        (md.body.sources || [])
          .find((source) => source.name === 'default')
          ?.tables.find(
            (table) => (table?.table as SchemaTable)?.name === 'user_table',
          )?.event_triggers?.[0],
      ).toMatchSnapshot({ name: 'Modify the shotest path to longest' });
    });

    // delete ET
    cy.intercept('POST', hgeUrl('/v1/metadata'), (req) => {
      if (JSON.stringify(req.body).includes('delete_event_trigger')) {
        req.alias = 'deleteTrigger';
      }
      req.continue();
    });

    cy.intercept('POST', cliUrl('/apis/migrate'), (req) => {
      if (JSON.stringify(req.body).includes('delete_event_trigger')) {
        req.alias = 'deleteTrigger';
      }
    });
    cy.findByRole('button', { name: 'Delete Event Trigger' }).click();
    cy.wait('@deleteTrigger');
  });
});

describe('Create event trigger with logest possible path', () => {
  before(() => {
    // Remove a leftover trigger first, then drop any table the previous suite's
    // interrupted `after` cleanup left behind, so `createTable` here cannot fail
    // with "relation already exists". (On the hosted job the shortest-path
    // suite's `after all` reported an uncaught app error, then this hook found
    // `user_table` still present. The original React error's nested cause was
    // not captured; fixture cleanup must not depend on its diagnosis.)
    deleteLeftoverEventTrigger();
    postgres.helpers.dropTableIfExists('user_table');

    // create a table first
    postgres.helpers.createTable('user_table');

    // Track the table as a prerequisite via the metadata API (see
    // postgres.helpers.trackTable) — deterministic setup, consistent with the
    // shortest-path suite. This before-hook no longer drives the Data-manager
    // "untracked"/Track-button UI flow; the event-trigger CRUD under test is
    // still exercised through the UI in the `it`.
    postgres.helpers.trackTable('user_table');
  });
  after(() => {
    // delete the table
    cy.log('**--- Delete the table');
    postgres.helpers.deleteTable('user_table');
  });
  xit('When the users create, modify and delete an Event trigger, everything should work', () => {
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**--- Step 1: Create an ET with longest path**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');

    cy.visit('/events/data/manage', {
      timeout: 10000,
      onBeforeLoad(win) {
        cy.stub(win, 'prompt').returns('event_trigger_test');
      },
    });

    // --------------------
    cy.log('**--- Click on the Create button of the ET panel**');
    cy.get('[data-testid=data-create-trigger]').click();

    // trigger name
    cy.log('**--- Type the event trigger name**');
    cy.findByPlaceholderText('trigger_name').type('event_trigger_test');

    // select source
    cy.log('**--- Select the DB');
    cy.get('[name="source"]').radixSelect('default');

    // select schema and table (a single "schema / table" select)
    cy.log('**--- Select the schema and table');
    cy.get('[name="tableName"]').radixSelect('public / user_table');

    // select the trigger operation
    cy.log('**--- Select the ET operation');
    cy.findByRole('checkbox', { name: 'Insert' }).click();

    // add webhook url
    cy.log('**--- Add webhook url');
    cy.get('[name=handler]').type('http://httpbin.org/post');

    // toggle the adv setting, add retry config and headers
    cy.log('**--- Add retry config');
    cy.findByText('Advanced Settings').click();
    cy.get('[name=num_retries]').clear().type('10');
    cy.get('[name=interval_sec]').clear().type('5');
    cy.get('[name=timeout_sec]').clear().type('70');

    // add headers
    cy.log('**--- Add header');
    cy.findByPlaceholderText('key').type('x-hasura-user-id');
    cy.findByPlaceholderText('value').type('1234');

    // add Sample context
    cy.log('**--- Click on show sample context and fill the form');
    cy.findByText('Show Sample Context').click();
    cy.findByPlaceholderText('Key...').type('env-var');
    cy.findByPlaceholderText('Value...').type('env-var-value');

    // add Request Options Transform
    cy.log('**--- Click on Add Request Options Transform and fill the form');
    cy.findByText('Add Request Options Transform').click();
    cy.findByRole('radio', { name: 'GET' }).click();
    cy.get('[name=request_url]').type('/transformUrl');
    // The request URL is propagated into the saved form state on a 1s debounce
    // (editorDebounceTime); saving before it fires drops `request_transform.url`
    // from the metadata. The read-only Preview field is rendered from the
    // propagated value (with `{{$base_url}}` resolved to the webhook), so wait
    // for it to show the final URL before continuing — a deterministic wait on
    // the real state, not a fixed delay.
    cy.get('[data-test=transform-requestUrl-preview]').should(
      'have.value',
      'http://httpbin.org/post/transformUrl',
    );
    cy.findAllByPlaceholderText('Key...').eq(2).type('x-hasura-user-id');
    cy.findAllByPlaceholderText('Value...').eq(2).type('my-user-id');

    // add Payload Transform
    cy.log('**--- Click on Add Payload Transform and fill the form');
    cy.findByText('Add Payload Transform').click();

    // click on create button to save ET
    cy.log('**--- Click on Create Event Trigger');
    cy.get('[data-test=trigger-create]').click();

    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**--- Step 2: Modify an ET to shortest path and save it**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');

    // modfy the trigger operation
    cy.log(
      '**--- Click on Edit trigger operation and modfiy the trigger operation',
    );
    cy.findAllByRole('button', { name: 'Edit' }).eq(1).click();
    cy.findByRole('checkbox', { name: 'Update' }).click();

    cy.findByRole('radio', { name: 'Choose columns' }).click();
    cy.findByRole('checkbox', { name: 'id' }).click();
    cy.findByRole('button', { name: 'Save' }).click();

    // remove Request Options Transform
    cy.log('**--- Click on Remove Request Options Transform');
    cy.findByText('Remove Request Options Transform').click();

    // remove Payload Transform
    cy.log('**--- Click on Remove Payload Transform');
    cy.findByText('Remove Payload Transform').click();

    // save the ET
    cy.log('**--- Click on Save Event trigger to modify the ET');
    cy.findByRole('button', { name: 'Save Event Trigger' }).click();

    readMetadata().then((md: { body: HasuraMetadataV3 }) => {
      cy.wrap(
        (md?.body?.sources || [])
          .find((source) => source?.name === 'default')
          ?.tables.find(
            (table) => (table?.table as SchemaTable)?.name === 'user_table',
          )?.event_triggers?.[0],
      ).toMatchSnapshot({ name: 'Modify the longest path to shortest path' });
    });

    // delete ET
    cy.intercept('POST', hgeUrl('/v1/metadata'), (req) => {
      if (JSON.stringify(req.body).includes('delete_event_trigger')) {
        req.alias = 'deleteTrigger';
      }
      req.continue();
    });

    cy.intercept('POST', cliUrl('/apis/migrate'), (req) => {
      if (JSON.stringify(req.body).includes('delete_event_trigger')) {
        req.alias = 'deleteTrigger';
      }
    });
    cy.findByRole('button', { name: 'Delete Event Trigger' }).click();
    cy.wait('@deleteTrigger');
  });
});
