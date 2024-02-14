import { HasuraMetadataV3 } from '@hasura/console-legacy-ce';
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
    cy.intercept('POST', 'http://localhost:8080/v1/metadata', req => {
      if (JSON.stringify(req.body).includes('add_remote_schema')) {
        req.alias = 'addRs';
      }
      req.continue();
    });
    cy.intercept('POST', 'http://localhost:9693/apis/migrate', req => {
      if (JSON.stringify(req.body).includes('add_remote_schema')) {
        req.alias = 'addRs';
      }
    });
    cy.log('**--- Click on Add Remote Schema');
    cy.findByRole('button', { name: 'Add Remote Schema' }).click();
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

    // edit RS webhook url
    cy.log('**--- Modify the webhoook URL');
    cy.get('[name=handler]')
      .clear()
      .type('https://countries.trevorblades.com/');

    // check forward client header
    cy.log('**--- Check forward client header');
    cy.findByText('Forward all headers from client').click();

    // add header
    cy.log('**--- Click on Add headers button and add some headers');
    cy.findAllByPlaceholderText('header name').type('user_id');
    cy.findAllByPlaceholderText('header value').type('1234');

    // add server timeout
    cy.log('**--- Add the gql server timeout');
    cy.get('[name=timeout_sec]').clear().type('80');

    // click on create button to save ET
    cy.intercept('POST', 'http://localhost:8080/v1/metadata', req => {
      if (JSON.stringify(req.body).includes('update_remote_schema')) {
        req.alias = 'updateRs';
      }
      req.continue();
    });
    cy.intercept('POST', 'http://localhost:9693/apis/migrate', req => {
      if (JSON.stringify(req.body).includes('update_remote_schema')) {
        req.alias = 'updateRs';
      }
    });
    cy.log('**--- Click on Add Remote Schema');
    cy.findByRole('button', { name: 'Save' }).click();
    cy.wait('@updateRs');

    // to again to modif tab
    cy.visit('/remote-schemas/manage/remote_schema_name/modify', {
      timeout: 10000,
      onBeforeLoad(win) {
        cy.stub(win, 'prompt').returns('remote_schema_name');
      },
    });

    // add gql customization
    cy.log('**--- Click on Edit and apply some customization');
    cy.findByRole('button', { name: 'Edit' }).click();
    cy.get(`[name=namespace]`).type('namespace_');
    cy.get(`[name=prefix]`).type('prefix_');
    cy.get(`[name=suffix]`).type('_suffix');
    cy.get('[name=type-name-lhs ]').select('Country');
    cy.get(`[name="type-name-rhs[0]"]`).type('country_name');
    cy.findByRole('button', { name: 'Add Field Mapping' }).click();
    cy.get('[name=field-type]').select('Continent');
    cy.get(`[name=field-type-prefix]`).type('prefix_');
    cy.get(`[name=field-type-suffix]`).type('_suffix');
    cy.get('[name=field-type-lhs]').select('code');
    cy.get(`[name="field-type-rhs[0]"]`).type('country_code');
    cy.findByRole('button', { name: 'Add Field Customization' }).click();

    // save the RS
    cy.log('**--- Click on Save to modify the RS');
    cy.intercept('POST', 'http://localhost:8080/v1/metadata', req => {
      if (JSON.stringify(req.body).includes('update_remote_schema')) {
        req.alias = 'updateRs';
      }
      req.continue();
    });
    cy.intercept('POST', 'http://localhost:9693/apis/migrate', req => {
      if (JSON.stringify(req.body).includes('update_remote_schema')) {
        req.alias = 'updateRs';
      }
    });
    cy.findByRole('button', { name: 'Save' }).click();
    cy.wait('@updateRs');

    cy.visit('/remote-schemas/manage/remote_schema_name/modify', {
      timeout: 10000,
      onBeforeLoad(win) {
        cy.stub(win, 'prompt').returns('remote_schema_name');
      },
    });

    readMetadata().then((md: { body: HasuraMetadataV3 }) => {
      cy.wrap(
        md.body?.remote_schemas?.find(rs => rs?.name === 'remote_schema_name')
      ).toMatchSnapshot({ name: 'Modify the shotest path to longest' });
    });

    // delete RS
    cy.log('**--- Click on Delete to delete the RS');
    cy.intercept('POST', 'http://localhost:8080/v1/metadata', req => {
      if (JSON.stringify(req.body).includes('remove_remote_schema')) {
        req.alias = 'removeRs';
      }
      req.continue();
    });
    cy.intercept('POST', 'http://localhost:9693/apis/migrate', req => {
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
    cy.get('[name=forward_client_headers]').click();

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
      'prefix_query_root'
    );
    cy.get(`[name="customization.query_root.suffix"]`).type(
      'query_root_suffix'
    );
    cy.get(`[name="customization.mutation_root.parent_type"]`).type(
      'mutation_root'
    );
    cy.get(`[name="customization.mutation_root.prefix"]`).type(
      'prefix_mutation_root'
    );
    cy.get(`[name="customization.mutation_root.suffix"]`).type(
      'mutation_root_suffix'
    );

    // click on create button to save ET

    cy.log('**--- Click on Add Remote Schema');
    cy.intercept('POST', 'http://localhost:8080/v1/metadata', req => {
      if (JSON.stringify(req.body).includes('add_remote_schema')) {
        req.alias = 'addRs';
      }
      req.continue();
    });
    cy.intercept('POST', 'http://localhost:9693/apis/migrate', req => {
      if (JSON.stringify(req.body).includes('add_remote_schema')) {
        req.alias = 'addRs';
      }
    });
    cy.findByRole('button', { name: 'Add Remote Schema' }).click();
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
    cy.get('[name=handler]')
      .clear()
      .type('https://countries.trevorblades.com/');

    // check forward client header
    cy.log('**--- Check forward client header');
    cy.findByText('Forward all headers from client').click();

    // clear gql timeout
    cy.log('**--- Clear the gql time out');
    cy.get('[name=timeout_sec]').clear();

    // clear comment
    cy.log('**--- Clear the RS comment');
    cy.get('[name=comment]').clear();

    // click on save button to save ET
    cy.intercept('POST', 'http://localhost:8080/v1/metadata', req => {
      if (JSON.stringify(req.body).includes('update_remote_schema')) {
        req.alias = 'updateRs';
      }
      req.continue();
    });
    cy.intercept('POST', 'http://localhost:9693/apis/migrate', req => {
      if (JSON.stringify(req.body).includes('update_remote_schema')) {
        req.alias = 'updateRs';
      }
    });
    cy.log('**--- Click on Add Remote Schema');
    cy.findByRole('button', { name: 'Save' }).click();
    cy.wait('@updateRs');

    cy.visit('/remote-schemas/manage/remote_schema_name/modify', {
      timeout: 10000,
      onBeforeLoad(win) {
        cy.stub(win, 'prompt').returns('remote_schema_name');
      },
    });

    readMetadata().then((md: { body: HasuraMetadataV3 }) => {
      cy.wrap(
        md.body?.remote_schemas?.find(rs => rs?.name === 'remote_schema_name')
      ).toMatchSnapshot({ name: 'Modify the shotest path to longest' });
    });

    // delete RS
    cy.log('**--- Click on Delete to delete the RS');
    cy.intercept('POST', 'http://localhost:8080/v1/metadata', req => {
      if (JSON.stringify(req.body).includes('remove_remote_schema')) {
        req.alias = 'removeRs';
      }
      req.continue();
    });
    cy.intercept('POST', 'http://localhost:9693/apis/migrate', req => {
      if (JSON.stringify(req.body).includes('remove_remote_schema')) {
        req.alias = 'removeRs';
      }
    });
    cy.findByRole('button', { name: 'Delete' }).click();
    cy.wait('@removeRs');
  });
});
