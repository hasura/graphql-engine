import { getElementFromAlias } from '../../../helpers/eventHelpers';
import { replaceMetadata, resetMetadata } from '../../../helpers/metadata';
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
    cy.visit(
      'http://localhost:3000/remote-schemas/manage/source_rs/relationships'
    );
    cy.get(getElementFromAlias('add-a-new-rs-relationship')).click();
    cy.get(getElementFromAlias('radio-select-remoteDB')).click();
    cy.get(getElementFromAlias('rs-to-db-rel-name')).type('RelationshipName');
    cy.get(getElementFromAlias('select-rel-type')).select('array');
    cy.get(getElementFromAlias('select-source-type')).select('Pokemon');
    cy.get(getElementFromAlias('select-ref-db')).select('default');
    cy.get(getElementFromAlias('select-ref-schema')).select('public');
    cy.get(getElementFromAlias('select-ref-table')).select('destination_table');
    cy.get(getElementFromAlias('select-source-field')).select('id');
    cy.get(getElementFromAlias('select-ref-col')).select('name');
    cy.get(getElementFromAlias('add-rs-relationship')).click();

    cy.get(getElementFromAlias('remote-schema-relationships-table')).should(
      'exist'
    );
    cy.get(getElementFromAlias('remote-schema-relationships-table'))
      .find('tr')
      .should('have.length', 2);
    cy.get(getElementFromAlias('remote-schema-relationships-table')).contains(
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
