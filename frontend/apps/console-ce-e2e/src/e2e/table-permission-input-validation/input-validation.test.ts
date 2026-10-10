import { HasuraMetadataV3, SchemaTable } from '@hasura/shared/types';
import { readMetadata } from '../actions/withTransform/utils/services/readMetadata';
import { postgres } from '../data/manage-database/postgres.spec';

describe('Create a insert type table permission with input validation', () => {
  before(() => {
    // create a table first
    postgres.helpers.createTable('user_table');

    // Track the table as a prerequisite via the metadata API (see
    // postgres.helpers.trackTable) — deterministic setup. The permission CRUD
    // under test is still driven through the UI in each `it`; only this setup
    // step moved off the UI, so these before-hooks no longer exercise the
    // Data-manager "untracked"/Track-button flow.
    postgres.helpers.trackTable('user_table');
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
      '**--- Step 1: Create an insert table permission with input validation**',
    );
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');

    cy.visit(
      '/data/manage/source/default/table/permissions?table=%7B%22name%22%3A%22user_table%22%2C%22schema%22%3A%22public%22%7D',
      {
        timeout: 10000,
      },
    );

    // --------------------
    cy.log('**--- Enter a new Role**');
    cy.get('[aria-label=create-new-role]').click().type('new_role');

    // --------------------
    cy.log('**--- Click to open permission form**');
    cy.get('button[aria-label=new_role-insert]').click();

    // --------------------
    cy.log('**--- Fill the validate form**');
    cy.findByText('Input Validation').click();
    cy.get('[data-testid="validateInput.enabled"]').click();
    cy.get('[name="validateInput.definition.url"]').type(
      'http://host.docker.internal',
    );

    // --------------------
    cy.log('**--- Click to save permission');
    cy.get('[data-testid=permissions-form-submit]').click();
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
    cy.get('button[aria-label=new_role-insert]').click();
    cy.findByText('Input Validation').click();
    cy.findByText('Forward client headers to webhook').click();
    cy.get('[name="validateInput.definition.timeout"]').clear().type('40');
    cy.findByRole('button', { name: 'Add Additional Headers' }).click();
    cy.findByPlaceholderText('Key...').type('x-hasura-user-id');
    cy.findByPlaceholderText('Value...').type('1234');

    // --------------------
    cy.log('**--- Click to Save Permission');
    cy.get('[data-testid=permissions-form-submit]').click();
    cy.wait(2000);

    readMetadata().then((md: { body: HasuraMetadataV3 }) => {
      cy.wrap(
        (md.body.sources || [])
          .find((source) => source.name === 'default')
          ?.tables.find(
            (table) => (table?.table as SchemaTable)?.name === 'user_table',
          )?.insert_permissions,
      ).toMatchSnapshot();
    });

    // delete Permission
    cy.get('button[aria-label=new_role-insert]').click();
    cy.findByRole('button', { name: 'Delete Permissions' }).click();
    cy.get('[data-testid=alert-confirm-button]').click();
  });
});

describe('Create a update type table permission with input validation', () => {
  before(() => {
    // create a table first
    postgres.helpers.createTable('user_table');

    // Track the table as a prerequisite via the metadata API (see
    // postgres.helpers.trackTable) — deterministic setup. The permission CRUD
    // under test is still driven through the UI in each `it`; only this setup
    // step moved off the UI, so these before-hooks no longer exercise the
    // Data-manager "untracked"/Track-button flow.
    postgres.helpers.trackTable('user_table');
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
      '**--- Step 1: Create an update table permission with input validation**',
    );
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');

    cy.visit(
      '/data/manage/source/default/table/permissions?table=%7B%22name%22%3A%22user_table%22%2C%22schema%22%3A%22public%22%7D',
      {
        timeout: 10000,
      },
    );

    // --------------------
    cy.log('**--- Enter a new Role**');
    cy.get('[aria-label=create-new-role]').click().type('new_role');

    // --------------------
    cy.log('**--- Click to open permission form**');
    cy.get('button[aria-label=new_role-update]').click();

    // --------------------
    cy.log('**--- Fill the validate form**');
    cy.findByText('Input Validation').click();
    cy.get('[data-testid="validateInput.enabled"]').click();
    cy.get('[name="validateInput.definition.url"]').type(
      'http://host.docker.internal',
    );

    // --------------------
    cy.log('**--- Click to save permission');
    cy.get('[data-testid=permissions-form-submit]').click();
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
    cy.get('button[aria-label=new_role-update]').click();
    cy.findByText('Input Validation').click();
    cy.findByText('Forward client headers to webhook').click();
    cy.get('[name="validateInput.definition.timeout"]').clear().type('40');
    cy.findByRole('button', { name: 'Add Additional Headers' }).click();
    cy.findByPlaceholderText('Key...').type('x-hasura-user-id');
    cy.findByPlaceholderText('Value...').type('1234');

    // --------------------
    cy.log('**--- Click to Save Permission');
    cy.get('[data-testid=permissions-form-submit]').click();
    cy.wait(2000);

    readMetadata().then((md: { body: HasuraMetadataV3 }) => {
      cy.wrap(
        (md.body.sources || [])
          .find((source) => source.name === 'default')
          ?.tables.find(
            (table) => (table?.table as SchemaTable)?.name === 'user_table',
          )?.update_permissions,
      ).toMatchSnapshot();
    });

    // delete Permission
    cy.get('button[aria-label=new_role-update]').click();
    cy.findByRole('button', { name: 'Delete Permissions' }).click();
    cy.get('[data-testid=alert-confirm-button]').click();
  });
});

describe('Create a delete type table permission with input validation', () => {
  before(() => {
    // create a table first
    postgres.helpers.createTable('user_table');

    // Track the table as a prerequisite via the metadata API (see
    // postgres.helpers.trackTable) — deterministic setup. The permission CRUD
    // under test is still driven through the UI in each `it`; only this setup
    // step moved off the UI, so these before-hooks no longer exercise the
    // Data-manager "untracked"/Track-button flow.
    postgres.helpers.trackTable('user_table');
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
      '**--- Step 1: Create an delete table permission with input validation**',
    );
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');

    cy.visit(
      '/data/manage/source/default/table/permissions?table=%7B%22name%22%3A%22user_table%22%2C%22schema%22%3A%22public%22%7D',
      {
        timeout: 10000,
      },
    );

    // --------------------
    cy.log('**--- Enter a new Role**');
    cy.get('[aria-label=create-new-role]').click().type('new_role');

    // --------------------
    cy.log('**--- Click to open permission form**');
    cy.get('button[aria-label=new_role-delete]').click();

    // --------------------
    cy.log('**--- Fill the validate form**');
    cy.findByText('Input Validation').click();
    cy.get('[data-testid="validateInput.enabled"]').click();
    cy.get('[name="validateInput.definition.url"]').type(
      'http://host.docker.internal',
    );

    // --------------------
    cy.log('**--- Click to save permission');
    cy.get('[data-testid=permissions-form-submit]').click();
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
    cy.get('button[aria-label=new_role-delete]').click();
    cy.findByText('Input Validation').click();
    cy.findByText('Forward client headers to webhook').click();
    cy.get('[name="validateInput.definition.timeout"]').clear().type('40');
    cy.findByRole('button', { name: 'Add Additional Headers' }).click();
    cy.findByPlaceholderText('Key...').type('x-hasura-user-id');
    cy.findByPlaceholderText('Value...').type('1234');

    // --------------------
    cy.log('**--- Click to Save Permission');
    cy.get('[data-testid=permissions-form-submit]').click();
    cy.wait(2000);

    readMetadata().then((md: { body: HasuraMetadataV3 }) => {
      cy.wrap(
        (md.body.sources || [])
          .find((source) => source.name === 'default')
          ?.tables.find(
            (table) => (table?.table as SchemaTable)?.name === 'user_table',
          )?.delete_permissions,
      ).toMatchSnapshot();
    });

    // delete Permission
    cy.get('button[aria-label=new_role-delete]').click();
    cy.findByRole('button', { name: 'Delete Permissions' }).click();
    cy.get('[data-testid=alert-confirm-button]').click();
  });
});
