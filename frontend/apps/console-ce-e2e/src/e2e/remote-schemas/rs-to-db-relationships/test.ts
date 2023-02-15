import { replaceMetadata, resetMetadata } from '../helpers/metadata';
import { postgres } from '../../data/manage-database/postgres.spec';

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

  it('verify creating a new rs-to-db relationship', () => {
    cy.visit('/remote-schemas/manage/source_rs/relationships');
    cy.get('[data-test=add-a-new-rs-relationship').click();
    cy.get('[data-test=radio-select-remoteDB').click();
    cy.get('[data-test=rs-to-db-rel-name').type('RelationshipName');
    cy.get('[data-test=select-rel-type').select('array');
    cy.get('[data-test=select-source-type').select('Pokemon');
    cy.get('[data-test=select-ref-db').select('default', { force: true });
    cy.get('[data-test=select-ref-schema').select('public');
    cy.get('[data-test=select-ref-table').select('destination_table');
    cy.get('[data-test=select-source-field').select('id');
    cy.get('[data-test=select-ref-col').select('name');
    cy.get('[data-test=add-rs-relationship').click();

    cy.get('[data-test=remote-schema-relationships-table').should('exist');
    cy.get('[data-test=remote-schema-relationships-table')
      .find('tr')
      .should('have.length', 2);
    cy.get('[data-test=remote-schema-relationships-table').contains(
      'td',
      'RelationshipName'
    );
  });

  after(() => {
    //  reset the metadata
    resetMetadata();

    // delete the table
    postgres.helpers.deleteTable('destination_table');
  });
});
