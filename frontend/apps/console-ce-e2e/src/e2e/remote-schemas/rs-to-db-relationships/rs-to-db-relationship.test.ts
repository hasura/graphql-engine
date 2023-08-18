import { replaceMetadata } from '../helpers/metadata';
import { postgres } from '../../data/manage-database/postgres.spec';
import { HasuraMetadataV3 } from '@hasura/console-legacy-ce';
import { readMetadata } from '../../actions/withTransform/utils/services/readMetadata';

describe('check if remote schema to db relationships are created properly', () => {
  before(() => {
    // create a table called destination_table
    postgres.helpers.createTable('destination_table');
    // load stuff into the metadata
    replaceMetadata({
      version: 3,
      sources: [
        {
          name: 'default',
          kind: 'postgres',
          tables: [
            {
              table: {
                schema: 'public',
                name: 'destination_table',
              },
            },
          ],
          configuration: {
            connection_info: {
              use_prepared_statements: true,
              database_url: {
                from_env: 'HASURA_GRAPHQL_DATABASE_URL',
              },
              isolation_level: 'read-committed',
              pool_settings: {
                connection_lifetime: 600,
                retries: 1,
                idle_timeout: 180,
                max_connections: 50,
              },
            },
          },
        },
      ],
      remote_schemas: [
        {
          name: 'source_rs',
          definition: {
            url: 'https://graphql-pokemon2.vercel.app/',
            timeout_seconds: 60,
          },
          comment: '',
        },
      ],
    });
  });

  after(() => {
    // delete the table
    postgres.helpers.deleteTable('destination_table');
  });

  it('verify creating a new rs-to-db relationship', () => {
    cy.visit('/remote-schemas/manage/source_rs/relationships');
    cy.findByText('Add a new relationship').click();
    cy.findByText('Remote Database').click();
    cy.get('[name=relationshipName]').type('RelationshipName');
    cy.get('[name=relationshipType]').select('array');
    cy.get('[aria-labelledby=typeName]')
      .focus() // workaround for selecting things with react-select
      .type('Pokemon{enter}', { force: true });
    cy.get('[aria-labelledby=target]')
      .focus() // workaround for selecting things with react-select
      .type('default / public / destination_table{enter}', { force: true });
    cy.get('[data-test=select-source-field').select('id');
    cy.get('[data-test=select-ref-col').select('name');
    cy.findByRole('button', { name: 'Add Relationship' }).click();

    cy.get('[data-test=remote-schema-relationships-table').should('exist');
    cy.get('[data-test=remote-schema-relationships-table')
      .find('tr')
      .should('have.length', 2);
    cy.get('[data-test=remote-schema-relationships-table').contains(
      'td',
      'RelationshipName'
    );
    readMetadata().then((md: { body: HasuraMetadataV3 }) => {
      cy.wrap(
        md.body?.remote_schemas?.find(rs => rs?.name === 'source_rs')
          ?.remote_relationships
      ).toMatchSnapshot({ name: 'rs-to-db-relationship' });
    });

    // delete rs-to-rs relationship
    cy.visit('/remote-schemas/manage/source_rs/relationships', {
      timeout: 10000,
      onBeforeLoad(win) {
        cy.stub(win, 'prompt').returns('RelationshipName');
      },
    });
    cy.findByRole('button', { name: 'Remove' }).click();

    //  delete both remote schemas
    cy.visit('/remote-schemas/manage/source_rs/modify', {
      timeout: 10000,
      onBeforeLoad(win) {
        cy.stub(win, 'prompt').returns('source_rs');
      },
    });
    cy.findByRole('button', { name: 'Delete' }).click();
  });
});
