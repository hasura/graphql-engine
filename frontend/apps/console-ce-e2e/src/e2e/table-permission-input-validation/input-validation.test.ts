import { HasuraMetadataV3 } from '@hasura/console-legacy-ce';
import { readMetadata } from '../actions/withTransform/utils/services/readMetadata';
import { postgres } from '../data/manage-database/postgres.spec';

describe('Create a insert type table permission with input validation', () => {
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

    // wait for loading to not be visible
    cy.get('span:contains("Loading...")', { timeout: 10000 }).should(
      'not.be.visible'
    );
  });
  after(() => {
    // delete the table
    cy.log('**--- Delete the table');
    postgres.helpers.deleteTable('user_table');
  });

  it('When the users create, modify and delete a insert type table permission everything should work', () => {
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log(
      '**--- Step 1: Create an insert table permission with input validation**'
    );
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');

    cy.visit('/data/default/schema/public/tables/user_table/permissions', {
      timeout: 10000,
    });

    // --------------------
    cy.log('**--- Enter a new Role**');
    cy.get('[data-test=role-textbox]').click().type('new_role');

    // --------------------
    cy.log('**--- Click to open permission form**');
    cy.get('[data-test=new_role-insert]').click();

    // --------------------
    cy.log('**--- Fill the validate form**');
    cy.findByText('Input Validation').click();
    cy.get('[data-testid=enableValidation]').click();
    cy.get('[name="definition.url"]').type('http://host.docker.internal');

    // --------------------
    cy.log('**--- Click to save permission');
    cy.get('[data-test=Save-Permissions-button]').click();
    // NOTE: will remove the wait time (have to merge PR because of release)
    cy.wait(2000);

    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**--- Step 2: Modify to add optional validation fields**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');

    // --------------------
    cy.log('**--- Add other validation args');
    cy.get('[data-test=new_role-insert]').click();
    cy.findByText('Input Validation').click();
    cy.findByText('Forward client headers to webhook').click();
    cy.get('[name="definition.timeout"]').clear().type('40');
    cy.findByRole('button', { name: 'Add Additional Headers' }).click();
    cy.findByPlaceholderText('Key').type('x-hasura-user-id');
    cy.findByPlaceholderText('Value or {{Environment_Variable}}').type('1234');

    // --------------------
    cy.log('**--- Click to Save Permission');
    cy.get('[data-test=Save-Permissions-button]').click();
    cy.wait(2000);

    readMetadata().then((md: { body: HasuraMetadataV3 }) => {
      cy.wrap(
        (md.body.sources || [])
          .find(source => source.name === 'default')
          .tables.find(table => table?.table?.name === 'user_table')
          ?.insert_permissions
      ).toMatchSnapshot();
    });

    // delete Permission
    cy.get('[data-test=new_role-insert]').click();
    cy.findByRole('button', { name: 'Delete Permissions' }).click();
  });
});

describe('Create a update type table permission with input validation', () => {
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

    // wait for loading to not be visible
    cy.get('span:contains("Loading...")', { timeout: 10000 }).should(
      'not.be.visible'
    );
  });
  after(() => {
    // delete the table
    cy.log('**--- Delete the table');
    postgres.helpers.deleteTable('user_table');
  });

  it('When the users create, modify and delete a update type table permission everything should work', () => {
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log(
      '**--- Step 1: Create an update table permission with input validation**'
    );
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');

    cy.visit('/data/default/schema/public/tables/user_table/permissions', {
      timeout: 10000,
    });

    // --------------------
    cy.log('**--- Enter a new Role**');
    cy.get('[data-test=role-textbox]').click().type('new_role');

    // --------------------
    cy.log('**--- Click to open permission form**');
    cy.get('[data-test=new_role-update]').click();

    // --------------------
    cy.log('**--- Fill the validate form**');
    cy.findByText('Input Validation').click();
    cy.get('[data-testid=enableValidation]').click();
    cy.get('[name="definition.url"]').type('http://host.docker.internal');

    // --------------------
    cy.log('**--- Click to save permission');
    cy.get('[data-test=Save-Permissions-button]').click();
    cy.wait(2000);

    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**--- Step 2: Modify to add optional validation fields**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');

    // --------------------
    cy.log('**--- Add other validation args');
    cy.get('[data-test=new_role-update]').click();
    cy.findByText('Input Validation').click();
    cy.findByText('Forward client headers to webhook').click();
    cy.get('[name="definition.timeout"]').clear().type('40');
    cy.findByRole('button', { name: 'Add Additional Headers' }).click();
    cy.findByPlaceholderText('Key').type('x-hasura-user-id');
    cy.findByPlaceholderText('Value or {{Environment_Variable}}').type('1234');

    // --------------------
    cy.log('**--- Click to Save Permission');
    cy.get('[data-test=Save-Permissions-button]').click();
    cy.wait(2000);

    readMetadata().then((md: { body: HasuraMetadataV3 }) => {
      cy.wrap(
        (md.body.sources || [])
          .find(source => source.name === 'default')
          .tables.find(table => table?.table?.name === 'user_table')
          ?.update_permissions
      ).toMatchSnapshot();
    });

    // delete Permission
    cy.get('[data-test=new_role-update]').click();
    cy.findByRole('button', { name: 'Delete Permissions' }).click();
  });
});

describe('Create a delete type table permission with input validation', () => {
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

    // wait for loading to not be visible
    cy.get('span:contains("Loading...")', { timeout: 10000 }).should(
      'not.be.visible'
    );
  });
  after(() => {
    // delete the table
    cy.log('**--- Delete the table');
    postgres.helpers.deleteTable('user_table');
  });

  it('When the users create, modify and delete a delete type table permission everything should work', () => {
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log(
      '**--- Step 1: Create an delete table permission with input validation**'
    );
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');

    cy.visit('/data/default/schema/public/tables/user_table/permissions', {
      timeout: 10000,
    });

    // --------------------
    cy.log('**--- Enter a new Role**');
    cy.get('[data-test=role-textbox]').click().type('new_role');

    // --------------------
    cy.log('**--- Click to open permission form**');
    cy.get('[data-test=new_role-delete]').click();

    // --------------------
    cy.log('**--- Fill the validate form**');
    cy.findByText('Input Validation').click();
    cy.get('[data-testid=enableValidation]').click();
    cy.get('[name="definition.url"]').type('http://host.docker.internal');

    // --------------------
    cy.log('**--- Click to save permission');
    cy.get('[data-test=Save-Permissions-button]').click();
    cy.wait(2000);

    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**--- Step 2: Modify to add optional validation fields**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');

    // --------------------
    cy.log('**--- Add other validation args');
    cy.get('[data-test=new_role-delete]').click();
    cy.findByText('Input Validation').click();
    cy.findByText('Forward client headers to webhook').click();
    cy.get('[name="definition.timeout"]').clear().type('40');
    cy.findByRole('button', { name: 'Add Additional Headers' }).click();
    cy.findByPlaceholderText('Key').type('x-hasura-user-id');
    cy.findByPlaceholderText('Value or {{Environment_Variable}}').type('1234');

    // --------------------
    cy.log('**--- Click to Save Permission');
    cy.get('[data-test=Save-Permissions-button]').click();
    cy.wait(2000);

    readMetadata().then((md: { body: HasuraMetadataV3 }) => {
      cy.wrap(
        (md.body.sources || [])
          .find(source => source.name === 'default')
          .tables.find(table => table?.table?.name === 'user_table')
          ?.delete_permissions
      ).toMatchSnapshot();
    });

    // delete Permission
    cy.get('[data-test=new_role-delete]').click();
    cy.findByRole('button', { name: 'Delete Permissions' }).click();
  });
});
