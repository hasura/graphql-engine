import { getElementFromAlias } from '../../../helpers/eventHelpers';
import { replaceMetadata, resetMetadata } from '../../../helpers/metadata';

describe('check if remote schema to db relationships are created properly', () => {
  before(() => {
    // load stuff into the metadata
    replaceMetadata({
      version: 3,
      sources: [
        {
          name: 'default',
          kind: 'postgres',
          tables: [],
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
        {
          name: 'ref_rs',
          definition: {
            url: 'https://hasura-console-test.herokuapp.com/v1/graphql/',
            timeout_seconds: 60,
          },
          comment: '',
        },
      ],
    });
  });

  it('should create a new rs-to-rs relationship from source field', () => {
    cy.visit(
      'http://localhost:3000/remote-schemas/manage/source_rs/relationships'
    );
    cy.get(getElementFromAlias('add-a-new-rs-relationship')).click();
    cy.get(getElementFromAlias('radio-select-remoteSchema')).click();
    cy.get(getElementFromAlias('rs-to-rs-rel-name')).type('RelationshipName');
    cy.get(getElementFromAlias('select-source-type')).select('Pokemon');
    cy.get(getElementFromAlias('select-ref-rs')).select('ref_rs');
    cy.get('.ant-tree-switcher').first().click();
    cy.get('.ant-tree-switcher').eq(1).click();
    cy.get('.ant-tree-checkbox').eq(1).click();
    cy.get(getElementFromAlias('select-argument')).select('Source Field');
    cy.get(getElementFromAlias('selet-source-field')).select('id');
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
  it('should create a new reverse rs-to-rs relationship with static fill value', () => {
    cy.visit(
      'http://localhost:3000/remote-schemas/manage/ref_rs/relationships'
    );
    cy.get(getElementFromAlias('add-a-new-rs-relationship')).click();
    cy.get(getElementFromAlias('radio-select-remoteSchema')).click();
    cy.get(getElementFromAlias('rs-to-rs-rel-name')).type(
      'StaticRelationshipName'
    );
    cy.get(getElementFromAlias('select-source-type')).select('test');
    cy.get(getElementFromAlias('select-ref-rs')).select('source_rs');
    cy.get('.ant-tree-switcher').first().click(); // expand Query
    cy.get('.ant-tree-switcher').eq(3).click(); // expand pokemon
    cy.get('.ant-tree-checkbox').eq(1).click(); // check name argument
    cy.get(getElementFromAlias('select-argument')).select('Static Value');
    cy.get(getElementFromAlias('select-static-value')).type('Bulbasaur');
    cy.get(getElementFromAlias('add-rs-relationship')).click();
    cy.get(getElementFromAlias('remote-schema-relationships-table')).should(
      'exist'
    );
    cy.get(getElementFromAlias('remote-schema-relationships-table'))
      .find('tr')
      .should('have.length', 2);
    cy.get(getElementFromAlias('remote-schema-relationships-table')).contains(
      'td',
      'StaticRelationshipName'
    );
  });

  after(() => {
    //  reset the metadata
    resetMetadata();
  });
});
