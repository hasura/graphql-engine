import { HasuraMetadataV3 } from '@hasura/shared/types';
import { cliUrl, hgeUrl } from '../../../support/endpoints';
import { readMetadata } from '../../actions/withTransform/utils/services/readMetadata';

describe('Create RS with shortest possible path', () => {
  it('When the users create, modify and delete a RS, everything should work', () => {
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**--- Step 1: Create a RS with shortest path**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');

    cy.visit('/remote-schemas/manage/schemas', {
      timeout: 10000,
      onBeforeLoad(win) {
        cy.stub(win, 'prompt').returns('remote_schema_name');
      },
    });

    // --------------------
    cy.log('**--- Click on the Add button of the RS panel**');
    cy.get('[data-testid=data-create-remote-schemas]').click();

    // RS name
    cy.log('**--- Type the RS name**');
    cy.get('[name=name]').type('remote_schema_name');

    // provide webhook URL
    cy.log('**--- Add webhook url');
    cy.get('[name="url.value"]').type('https://graphql-pokemon2.vercel.app');

    // click on create button to save ET
    cy.intercept('POST', hgeUrl('/v1/metadata'), (req) => {
      if (JSON.stringify(req.body).includes('add_remote_schema')) {
        req.alias = 'addRs';
      }
      req.continue();
    });
    cy.intercept('POST', cliUrl('/apis/migrate'), (req) => {
      if (JSON.stringify(req.body).includes('add_remote_schema')) {
        req.alias = 'addRs';
      }
    });
    cy.log('**--- Click on Create Remote Schema');
    cy.findByRole('button', { name: 'Create Remote Schema' }).click();
    cy.wait('@addRs');

    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**--- Step 2: Modify RS to longest path and save it**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');

    // go to modify tab of RS
    cy.visit('/remote-schemas/manage/remote_schema_name/modify', {
      timeout: 10000,
      onBeforeLoad(win) {
        cy.stub(win, 'prompt').returns('remote_schema_name');
      },
    });

    const interceptUpdateRs = () => {
      cy.intercept('POST', hgeUrl('/v1/metadata'), (req) => {
        if (JSON.stringify(req.body).includes('update_remote_schema')) {
          req.alias = 'updateRs';
        }
        req.continue();
      });
      cy.intercept('POST', cliUrl('/apis/migrate'), (req) => {
        if (JSON.stringify(req.body).includes('update_remote_schema')) {
          req.alias = 'updateRs';
        }
      });
    };

    // edit RS webhook url (the form is disabled until the RS has loaded)
    cy.log('**--- Modify the webhoook URL');
    cy.get('[name="url.value"]')
      .should('be.enabled')
      .clear()
      .type('https://countries.trevorblades.com/');

    // check forward client header
    cy.log('**--- Check forward client header');
    cy.get('[data-testid=forward_client_headers]').click();

    // add header
    cy.log('**--- Click on Add headers button and add some headers');
    cy.findByRole('button', { name: 'Add additional headers' }).click();
    cy.get(`[name="headers[0].name"]`).type('user_id');
    cy.get(`[name="headers[0].value"]`).type('1234');

    // add server timeout
    cy.log('**--- Add the gql server timeout');
    cy.get('[name=timeout_seconds]').clear().type('80');

    cy.log('**--- Click on Save Remote Schema');
    interceptUpdateRs();
    cy.findByRole('button', { name: 'Save Remote Schema' }).click();
    cy.wait('@updateRs');

    // The console no longer has an editor for per-type/per-field name
    // mappings (removed with the legacy RS modify page), so set them through
    // the metadata API. Editing the customization below must keep them.
    cy.log('**--- Add type/field name mappings via the metadata API');
    readMetadata().then((md: { body: HasuraMetadataV3 }) => {
      const rs = md.body.remote_schemas?.find(
        (r) => r?.name === 'remote_schema_name',
      );
      cy.request('POST', hgeUrl('/v1/metadata'), {
        type: 'update_remote_schema',
        args: {
          name: 'remote_schema_name',
          comment: rs?.comment ?? '',
          definition: {
            ...rs?.definition,
            customization: {
              type_names: { mapping: { Country: 'country_name' } },
              field_names: [
                {
                  parent_type: 'Continent',
                  prefix: 'prefix_',
                  suffix: '_suffix',
                  mapping: { code: 'country_code' },
                },
              ],
            },
          },
        },
      });
    });

    // go again to modify tab
    cy.visit('/remote-schemas/manage/remote_schema_name/modify', {
      timeout: 10000,
      onBeforeLoad(win) {
        cy.stub(win, 'prompt').returns('remote_schema_name');
      },
    });

    // add gql customization
    cy.log('**--- Click on Add GQL Customization and apply some customization');
    cy.get('[name="url.value"]').should('be.enabled');
    cy.findByRole('button', { name: 'Add GQL Customization' }).click();
    cy.get('[name="customization.root_fields_namespace"]').type('namespace_');
    cy.get('[name="customization.type_prefix"]').type('prefix_');
    cy.get('[name="customization.type_suffix"]').type('_suffix');

    // save the RS
    cy.log('**--- Click on Save to modify the RS');
    interceptUpdateRs();
    cy.findByRole('button', { name: 'Save Remote Schema' }).click();
    cy.wait('@updateRs');

    cy.visit('/remote-schemas/manage/remote_schema_name/modify', {
      timeout: 10000,
      onBeforeLoad(win) {
        cy.stub(win, 'prompt').returns('remote_schema_name');
      },
    });

    readMetadata().then((md: { body: HasuraMetadataV3 }) => {
      cy.wrap(
        md.body?.remote_schemas?.find(
          (rs) => rs?.name === 'remote_schema_name',
        ),
      ).toMatchSnapshot({ name: 'Modify the shotest path to longest' });
    });

    // delete RS
    cy.log('**--- Click on Delete to delete the RS');
    cy.intercept('POST', hgeUrl('/v1/metadata'), (req) => {
      if (JSON.stringify(req.body).includes('remove_remote_schema')) {
        req.alias = 'removeRs';
      }
      req.continue();
    });
    cy.intercept('POST', cliUrl('/apis/migrate'), (req) => {
      if (JSON.stringify(req.body).includes('remove_remote_schema')) {
        req.alias = 'removeRs';
      }
    });
    cy.findByRole('button', { name: 'Delete' }).click();
    cy.wait('@removeRs');
  });
});

describe('Create RS with longest possible path', () => {
  it('When the users create, modify and delete a RS, everything should work', () => {
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**--- Step 1: Create a RS with longest path**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');

    cy.visit('/remote-schemas/manage/schemas', {
      timeout: 10000,
      onBeforeLoad(win) {
        cy.stub(win, 'prompt').returns('remote_schema_name');
      },
    });

    // --------------------
    cy.log('**--- Click on the Add button of the RS panel**');
    cy.get('[data-testid=data-create-remote-schemas]').click();

    // RS name
    cy.log('**--- Type the RS name**');
    cy.get('[name="name"]').type('remote_schema_name');

    // add comment
    cy.log('**--- Add comment to RS**');
    cy.get(`[name=comment]`).type('RS comment');

    // provide webhook URL
    cy.log('**--- Add webhook url');
    cy.get('[name="url.value"]').type('https://graphql-pokemon2.vercel.app');

    // add server timeout
    cy.log('**--- Add the gql server timeout');
    cy.get('[name=timeout_seconds]').type('80');

    // check forward client header
    cy.log('**--- Check forward client header');
    // Radix checkbox: the `name` sits on a hidden native input
    // (pointer-events: none); the clickable control carries the test id.
    cy.get('[data-testid=forward_client_headers]').click();

    // add header
    cy.log('**--- Click on Add headers button and add some headers');
    cy.findByRole('button', { name: 'Add additional headers' }).click();
    cy.get(`[name="headers[0].name"]`).type('user_id');
    cy.get(`[name="headers[0].value"]`).type('1234');

    // add gql customization
    cy.log('**--- Click on Add GQL Customization and apply some customization');
    cy.findByRole('button', { name: 'Add GQL Customization' }).click();
    cy.get(`[name="customization.root_fields_namespace"]`).type('namespace_');
    cy.get(`[name="customization.type_prefix"]`).type('prefix_');
    cy.get(`[name="customization.type_suffix"]`).type('_suffix');
    cy.get(`[name="customization.query_root.parent_type"]`).type('query_root');
    cy.get(`[name="customization.query_root.prefix"]`).type(
      'prefix_query_root',
    );
    cy.get(`[name="customization.query_root.suffix"]`).type(
      'query_root_suffix',
    );
    cy.get(`[name="customization.mutation_root.parent_type"]`).type(
      'mutation_root',
    );
    cy.get(`[name="customization.mutation_root.prefix"]`).type(
      'prefix_mutation_root',
    );
    cy.get(`[name="customization.mutation_root.suffix"]`).type(
      'mutation_root_suffix',
    );

    // click on create button to save ET

    cy.log('**--- Click on Add Remote Schema');
    cy.intercept('POST', hgeUrl('/v1/metadata'), (req) => {
      if (JSON.stringify(req.body).includes('add_remote_schema')) {
        req.alias = 'addRs';
      }
      req.continue();
    });
    cy.intercept('POST', cliUrl('/apis/migrate'), (req) => {
      if (JSON.stringify(req.body).includes('add_remote_schema')) {
        req.alias = 'addRs';
      }
    });
    cy.findByRole('button', { name: 'Create Remote Schema' }).click();
    cy.wait('@addRs');

    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**--- Step 2: Modify RS to shortest path and save it**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');

    // go to modify tab of RS
    cy.visit('/remote-schemas/manage/remote_schema_name/modify', {
      timeout: 10000,
      onBeforeLoad(win) {
        cy.stub(win, 'prompt').returns('remote_schema_name');
      },
    });

    // edit RS webhook url
    cy.log('**--- Modify the webhoook URL');
    cy.get('[name="url.value"]')
      .should('be.enabled')
      .clear()
      .type('https://countries.trevorblades.com/');

    // check forward client header
    cy.log('**--- Check forward client header');
    cy.get('[data-testid=forward_client_headers]').click();

    // clear gql timeout
    cy.log('**--- Clear the gql time out');
    cy.get('[name=timeout_seconds]').clear();

    // clear comment
    cy.log('**--- Clear the RS comment');
    cy.get('[name=comment]').clear();

    // click on save button to save ET
    cy.intercept('POST', hgeUrl('/v1/metadata'), (req) => {
      if (JSON.stringify(req.body).includes('update_remote_schema')) {
        req.alias = 'updateRs';
      }
      req.continue();
    });
    cy.intercept('POST', cliUrl('/apis/migrate'), (req) => {
      if (JSON.stringify(req.body).includes('update_remote_schema')) {
        req.alias = 'updateRs';
      }
    });
    cy.log('**--- Click on Save Remote Schema');
    cy.findByRole('button', { name: 'Save Remote Schema' }).click();
    cy.wait('@updateRs');

    cy.visit('/remote-schemas/manage/remote_schema_name/modify', {
      timeout: 10000,
      onBeforeLoad(win) {
        cy.stub(win, 'prompt').returns('remote_schema_name');
      },
    });

    readMetadata().then((md: { body: HasuraMetadataV3 }) => {
      cy.wrap(
        md.body?.remote_schemas?.find(
          (rs) => rs?.name === 'remote_schema_name',
        ),
      ).toMatchSnapshot({ name: 'Modify the shotest path to longest' });
    });

    // delete RS
    cy.log('**--- Click on Delete to delete the RS');
    cy.intercept('POST', hgeUrl('/v1/metadata'), (req) => {
      if (JSON.stringify(req.body).includes('remove_remote_schema')) {
        req.alias = 'removeRs';
      }
      req.continue();
    });
    cy.intercept('POST', cliUrl('/apis/migrate'), (req) => {
      if (JSON.stringify(req.body).includes('remove_remote_schema')) {
        req.alias = 'removeRs';
      }
    });
    cy.findByRole('button', { name: 'Delete' }).click();
    cy.wait('@removeRs');
  });
});
