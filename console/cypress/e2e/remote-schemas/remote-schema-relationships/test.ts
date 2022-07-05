import { getElementFromAlias } from '../../../helpers/eventHelpers';
import { replaceMetadata, resetMetadata } from '../../../helpers/metadata';
import { postgres } from '../../data/manage-database/postgres.spec';

describe('check if remote schema relationships are displayed properly', () => {
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
          name: 'destination_rs',
          definition: {
            url: 'https://graphql-pokemon2.vercel.app/',
            timeout_seconds: 60,
          },
          comment: '',
        },
        {
          name: 'source_rs',
          definition: {
            url: 'https://countries.trevorblades.com/',
            timeout_seconds: 60,
          },
          comment: '',
          remote_relationships: [
            {
              relationships: [
                {
                  definition: {
                    to_source: {
                      relationship_type: 'object',
                      source: 'default',
                      table: 'destination_table',
                      field_mapping: {
                        code: 'id',
                      },
                    },
                  },
                  name: 'an_example_rs_to_db_relationship',
                },
                {
                  definition: {
                    to_remote_schema: {
                      remote_field: {
                        pokemons: {
                          arguments: {},
                        },
                      },
                      remote_schema: 'destination_rs',
                      lhs_fields: ['code'],
                    },
                  },
                  name: 'an_example_rs_to_rs_relationship',
                },
              ],
              type_name: 'Country',
            },
          ],
        },
      ],
    });
  });

  it('verify if the rows exist on the remote schema table', () => {
    cy.visit(
      'http://localhost:3000/remote-schemas/manage/source_rs/relationships'
    );
    cy.get(getElementFromAlias('remote-schema-relationships-table')).should(
      'exist'
    );
    cy.get(getElementFromAlias('remote-schema-relationships-table'))
      .find('tr')
      .should('have.length', 3);
    cy.get(getElementFromAlias('remote-schema-relationships-table')).contains(
      'td',
      'an_example_rs_to_db_relationship'
    );
    cy.get(getElementFromAlias('remote-schema-relationships-table')).contains(
      'td',
      'an_example_rs_to_rs_relationship'
    );
  });

  after(() => {
    //  reset the metadata
    resetMetadata();

    // delete the table
    postgres.helpers.deleteTable('destination_table');
  });
});
