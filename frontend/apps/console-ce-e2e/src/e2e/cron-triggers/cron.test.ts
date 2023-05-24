import { HasuraMetadataV3 } from '@hasura/console-legacy-ce';
import { readMetadata } from '../actions/withTransform/utils/services/readMetadata';

describe('Create Cron trigger with shortest possible path', () => {
  it('When the users create, modify to longest path and delete an Cron trigger, everything should work', () => {
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**--- Step 1: Create an Cron trigger with shortest path**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');

    cy.visit('/events/cron/manage', {
      timeout: 10000,
      onBeforeLoad(win) {
        cy.stub(win, 'prompt').returns('cron_trigger_name');
      },
    });

    // click on create cron trigger button
    cy.log('**--- Click on the create Cron triger of the cron trigger panel**');
    cy.get('[data-test="create-cron-trigger"]').click();

    // trigger name
    cy.log('**--- Type the cron trigger name**');
    cy.findByPlaceholderText('Name...').type('cron_trigger_name');

    // add webhook url
    cy.log('**--- Add webhook url');
    cy.get('[name=webhook]').type('http://httpbin.org/post');

    // select cron schedule
    cy.log('**--- Add cron schedule');
    cy.findAllByRole('button', { name: 'Frequently used crons' }).click();
    cy.findByText('Every minute').click();

    // add payload
    cy.log('**--- Add request payload');
    cy.get('.ace_content').type('{{}"name":"json_payload"}');

    // click on create button to save ET
    cy.log('**--- Click on Add Cron Trigger');
    cy.findAllByRole('button', { name: 'Add Cron Trigger' }).click();

    cy.log('**--- Wait for metadata update to finish **');
    cy.intercept('POST', '**/v1/metadata').as('metadataRequest');
    cy.wait('@metadataRequest');

    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**--- Step 2: Modify Cron tigger to longest path and save it**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');

    // add comment
    cy.log('**--- Add comment to cron trigger**');
    cy.get('[name=comment]').type('my comment');

    // modify cron schedule
    cy.log('**--- Add cron schedule');
    cy.findAllByRole('button', { name: 'Frequently used crons' }).click();
    cy.findByText('Every 10 minutes').click();

    // open advance settings and add header, retry config
    cy.log('**--- Add headers and retry config');
    cy.findByText('Advanced Settings').click();
    cy.findAllByRole('button', { name: 'Add request headers' }).click();
    cy.findByPlaceholderText('Key...').type('user_id');
    cy.findByPlaceholderText('Value...').type('1234');
    cy.get('[name=num_retries]').clear().type('3');
    cy.get('[name=retry_interval_seconds]').clear().type('20');
    cy.get('[name=timeout_seconds]').clear().type('80');
    cy.get('[name=tolerance_seconds]').clear().type('80');

    // add Sample context
    cy.log('**--- Click on show sample context and fill the form');
    cy.findByText('Show Sample Context').click();
    cy.findAllByPlaceholderText('Key...').eq(1).type('env-var');
    cy.findAllByPlaceholderText('Value...').eq(1).type('env-var-value');

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
    cy.log('**--- Click on Update Cron Trigger');
    cy.findAllByRole('button', { name: 'Update Cron Trigger' }).click();

    readMetadata().then((md: { body: HasuraMetadataV3 }) => {
      cy.wrap(
        md.body?.cron_triggers.find(cron => cron.name === 'cron_trigger_name')
      ).toMatchSnapshot({ name: 'Modify the shotest path to longest' });
    });

    // delete cron trigger
    cy.log('**--- Click on Delete trigger');
    cy.findAllByRole('button', { name: 'Delete trigger' }).click();
  });
});
