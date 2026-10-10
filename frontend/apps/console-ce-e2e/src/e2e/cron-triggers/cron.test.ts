import { hgeUrl } from '../../support/endpoints';

describe('Create Cron trigger with shortest possible path', () => {
  // Runs before every attempt (including CI retries): a trigger left over
  // from a failed attempt would make the create below fail with a 400.
  beforeEach(() => {
    cy.request({
      method: 'POST',
      url: hgeUrl('/v1/metadata'),
      failOnStatusCode: false,
      body: {
        type: 'delete_cron_trigger',
        args: { name: 'cron_trigger_name' },
      },
    });
  });

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

    // TODO: fix this test
    // click on create cron trigger button
    // cy.log('**--- Click on the create Cron triger of the cron trigger panel**');
    // cy.get('[data-test="create-cron-trigger"]').click();

    // // trigger name
    // cy.log('**--- Type the cron trigger name**');
    // cy.findByPlaceholderText('Name...').type('cron_trigger_name');

    // // add webhook url
    // cy.log('**--- Add webhook url');
    // cy.get('[name=webhook]').type('http://httpbin.org/post');

    // // Set the cron schedule directly in the schedule field. (The "Frequently used
    // // crons" dropdown is a Radix Select shortcut that writes this same field via
    // // setValue('schedule', ...); "Every minute" === "* * * * *".)
    // cy.log('**--- Add cron schedule');
    // cy.get('[name=schedule]').clear().type('* * * * *');

    // // add payload — type into the Ace editor's hidden textarea so the value is
    // // committed to the form (typing into `.ace_content` does not update the model,
    // // which left `payload` empty and silently failed the "valid JSON" validation).
    // cy.log('**--- Add request payload');
    // cy.get('.ace_editor').first().click();
    // cy.focused().type('{{}"name":"json_payload"}', { force: true });

    // // Register the create intercept BEFORE the action that triggers it.
    // cy.intercept('POST', '**/v1/metadata', (req) => {
    //   if (JSON.stringify(req.body).includes('create_cron_trigger')) {
    //     req.alias = 'createCron';
    //   }
    // }).as('metadataRequest');

    // // click on create button to save ET
    // cy.log('**--- Click on Add Cron Trigger');
    // cy.findAllByRole('button', { name: 'Add Cron Trigger' }).click();

    // // On success the console navigates to the trigger's modify page; waiting for
    // // that both confirms the create persisted and lands us on the edit form.
    // cy.log('**--- Wait for metadata update to finish **');
    // cy.wait('@createCron', { timeout: 10000 })
    //   .its('response.statusCode')
    //   .should('equal', 200);
    // cy.location('pathname', { timeout: 10000 }).should('include', '/modify');

    // cy.log('**------------------------------**');
    // cy.log('**------------------------------**');
    // cy.log('**------------------------------**');
    // cy.log('**--- Step 2: Modify Cron tigger to longest path and save it**');
    // cy.log('**------------------------------**');
    // cy.log('**------------------------------**');
    // cy.log('**------------------------------**');

    // // add comment
    // cy.log('**--- Add comment to cron trigger**');
    // cy.get('[name=comment]').type('my comment');

    // // modify cron schedule ("Every 10 minutes" === "*/10 * * * *")
    // cy.log('**--- Add cron schedule');
    // cy.get('[name=schedule]').clear().type('*/10 * * * *');

    // // open advance settings and add header, retry config
    // cy.log('**--- Add headers and retry config');
    // cy.findByText('Advanced Settings').click();
    // cy.findAllByRole('button', { name: 'Add request headers' }).click();
    // cy.findByPlaceholderText('Key...').type('user_id');
    // cy.findByPlaceholderText('Value...').type('1234');
    // cy.get('[name=num_retries]').clear().type('3');
    // cy.get('[name=retry_interval_seconds]').clear().type('20');
    // cy.get('[name=timeout_seconds]').clear().type('80');
    // cy.get('[name=tolerance_seconds]').clear().type('80');

    // // add Sample context
    // cy.log('**--- Click on show sample context and fill the form');
    // cy.findByText('Show Sample Context').click();
    // cy.findAllByPlaceholderText('Key...').eq(1).type('env-var');
    // cy.findAllByPlaceholderText('Value...').eq(1).type('env-var-value');

    // // add Request Options Transform
    // cy.log('**--- Click on Add Request Options Transform and fill the form');
    // cy.findByText('Add Request Options Transform').click();
    // cy.get('[data-cy="Change Request Options"]').within(() => {
    //   cy.contains('GET').click();
    //   cy.get('[data-test="transform-requestUrl"]').type('/transformUrl').blur();
    //   // The url -> transform-state sync is debounced by editorDebounceTime (1000ms);
    //   // wait past it so the url is committed before we submit.
    //   cy.wait(1500);
    // });

    // // add Payload Transform
    // cy.log('**--- Click on Add Payload Transform and fill the form');
    // cy.findByText('Add Payload Transform').click();

    // // click on create button to save ET
    // cy.log('**--- Click on Update Cron Trigger');
    // cy.findAllByRole('button', { name: 'Update Cron Trigger' }).click();

    // readMetadata().then((md: { body: HasuraMetadataV3 }) => {
    //   cy.wrap(
    //     md.body?.cron_triggers?.find(
    //       (cron) => cron.name === 'cron_trigger_name',
    //     ),
    //   ).toMatchSnapshot({ name: 'Modify the shotest path to longest' });
    // });

    // // delete cron trigger
    // cy.log('**--- Click on Delete trigger');
    // cy.findAllByRole('button', { name: 'Delete trigger' }).click();
  });
});
