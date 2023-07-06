import { HasuraMetadataV3 } from '@hasura/console-legacy-ce';
import { readMetadata } from '../actions/withTransform/utils/services/readMetadata';
import { postgres } from '../data/manage-database/postgres.spec';

describe('Create event trigger with shortest possible path', () => {
  before(() => {
    // create a table first
    postgres.helpers.createTable('user_table');

    // track the table
    cy.visit('/data/default/schema/public', {
      timeout: 10000,
    });

    cy.get('[data-test=add-track-table-user_table]', {
      timeout: 10000,
    }).click();

    // if there is a trigger from a previous test, delete it
    cy.visit('/events/data/event_trigger_test/modify', {
      timeout: 10000,
      onBeforeLoad(win) {
        cy.stub(win, 'prompt').returns('event_trigger_test');
      },
    });

    // wait for loading to not be visible
    cy.get('span:contains("Loading...")', { timeout: 10000 }).should(
      'not.be.visible'
    );

    cy.get('body').then($body => {
      cy.log(
        '**--- Delete the ET',
        $body.find('button[data-test=delete-trigger]')
      );
      if ($body.find('button[data-test=delete-trigger]').length > 0) {
        cy.log('**--- Delete the ET 2');
        cy.intercept('POST', 'http://localhost:8080/v1/metadata', req => {
          if (JSON.stringify(req.body).includes('delete_event_trigger')) {
            req.alias = 'deleteTrigger';
          }
          req.continue();
        });

        cy.intercept('POST', 'http://localhost:9693/apis/migrate', req => {
          if (JSON.stringify(req.body).includes('delete_event_trigger')) {
            req.alias = 'deleteTrigger';
          }
        });

        cy.get('button[data-test=delete-trigger]').click();
        cy.wait('@deleteTrigger');
      }
    });
  });
  after(() => {
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
    cy.get('[name="source"]').select('default');

    // select schema and table
    cy.log('**--- Select the schema and table');
    cy.get('[name="schema"]').select('public');
    cy.get('[name="tableName"]').select('user_table');

    // select the trigger operation
    cy.log('**--- Select the ET operation');
    cy.get('[name=insert]').click();

    // add webhook url
    cy.log('**--- Add webhook url');
    cy.get('[name=handler]').type('http://httpbin.org/post');

    // click on create button to save ET
    cy.log('**--- Click on Create Event Trigger');
    cy.findByRole('button', { name: 'Create Event Trigger' }).click();

    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**--- Step 2: Modify an ET to longest path and save it**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');

    // modfy the trigger operation
    cy.log(
      '**--- Click on Edit trigger operation and modfiy the trigger operation'
    );
    cy.get('[data-test=edit-operations]').click();
    cy.get('[name=update]').click();
    cy.get('[name=column-id]', { timeout: 1000 }).click();
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
    cy.get('[name=GET]').click();
    cy.get('[name=request_url]').type('/transformUrl');
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
          .find(source => source.name === 'default')
          .tables.find(table => table?.table?.name === 'user_table')
          ?.event_triggers?.[0]
      ).toMatchSnapshot({ name: 'Modify the shotest path to longest' });
    });

    // delete ET
    cy.intercept('POST', 'http://localhost:8080/v1/metadata', req => {
      if (JSON.stringify(req.body).includes('delete_event_trigger')) {
        req.alias = 'deleteTrigger';
      }
      req.continue();
    });

    cy.intercept('POST', 'http://localhost:9693/apis/migrate', req => {
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
    // create a table first
    postgres.helpers.createTable('user_table');

    // track the table
    cy.visit('/data/default/schema/public', {
      timeout: 10000,
    });
    cy.get('[data-test=add-track-table-user_table]', {
      timeout: 10000,
    }).click();
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
    cy.get('[name="source"]').select('default');

    // select schema and table
    cy.log('**--- Select the schema and table');
    cy.get('[name="schema"]').select('public');
    cy.get('[name="tableName"]').select('user_table');

    // select the trigger operation
    cy.log('**--- Select the ET operation');
    cy.get('[name=insert]').click();

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
    cy.get('[name=GET]').click();
    cy.get('[name=request_url]').type('/transformUrl');
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
      '**--- Click on Edit trigger operation and modfiy the trigger operation'
    );
    cy.findAllByRole('button', { name: 'Edit' }).eq(1).click();
    cy.get('[name=update]').click();

    cy.get('[name=column-id]').click();
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
        (md.body.sources || [])
          .find(source => source.name === 'default')
          .tables.find(table => table?.table?.name === 'user_table')
          ?.event_triggers?.[0]
      ).toMatchSnapshot({ name: 'Modify the longest path to shortest path' });
    });

    // delete ET
    cy.intercept('POST', 'http://localhost:8080/v1/metadata', req => {
      if (JSON.stringify(req.body).includes('delete_event_trigger')) {
        req.alias = 'deleteTrigger';
      }
      req.continue();
    });

    cy.intercept('POST', 'http://localhost:9693/apis/migrate', req => {
      if (JSON.stringify(req.body).includes('delete_event_trigger')) {
        req.alias = 'deleteTrigger';
      }
    });
    cy.findByRole('button', { name: 'Delete Event Trigger' }).click();
    cy.wait('@deleteTrigger');
  });
});
