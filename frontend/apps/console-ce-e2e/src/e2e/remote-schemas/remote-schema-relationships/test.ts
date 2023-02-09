import type { Metadata } from '@hasura/console-legacy-ce';
import { replaceMetadata, resetMetadata } from '../helpers/metadata';
import { postgres } from '../../data/manage-database/postgres.spec';

describe('check if remote schema relationships are displayed properly', () => {
  before(() => {
    // create a table called destination_table
    postgres.helpers.createTable('destination_table');

    const metadata: Metadata['metadata'] = {
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
              // @ts-expect-error Originally...
              // 1. The whole metadata object used in this test was not typed.
              // 2. As a result, it went outdated compared to the Metadata type used in the app.
              // 3. By adding the type to the object, we realized the `relationships` property does not exist in the type.
              // 4. If you try to align the object to the type, the application does not work anymore.
              // It means that the Metadata type used here (the most modern one, theoretically) is not correct.
              //
              // Possible solutions:
              // 1. Add a @ts-expect error
              // 2. Fix the problem, that means reverse-engineering the server response and checking the docs, identifying the discrepancies among the Console-defined Metadata types (we have more than one...), fixing the app, etc.
              // 3. Fix the problem, nut only once we can leverage the server-exported Metadata type (produced by the OpenAPI specs).
              // Since 3 will happen in a near future, 1 is the best temporary choice for now.
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
    };
    // load stuff into the metadata
    replaceMetadata(metadata);
  });

  it('verify if the rows exist on the remote schema table', () => {
    cy.visit('/remote-schemas/manage/source_rs/relationships');
    cy.get('[data-test=remote-schema-relationships-table]').should('exist');
    cy.get('[data-test=remote-schema-relationships-table]')
      .find('tr')
      .should('have.length', 3);
    cy.get('[data-test=remote-schema-relationships-table]').contains(
      'td',
      'an_example_rs_to_db_relationship'
    );
    cy.get('[data-test=remote-schema-relationships-table]').contains(
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
