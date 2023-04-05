import type { Metadata } from '@hasura/console-legacy-ce';
import { readMetadata } from '../../../utils/checkMetadataPayload';

xdescribe('Dynamic Db Routing', () => {
  before(() => {
    cy.visit('/data/manage/connect/');

    cy.get('[data-test="database-display-name"]', {
      timeout: 10000,
    }).type('dynamic-db-routing');
    // click on radio button with name "connection-type" and value "ENVIRONMENT_VARIABLES"
    cy.get('[name="connection-type"]').check('ENVIRONMENT_VARIABLES');
    cy.get('[data-test="database-url-env"]').type(
      'HASURA_GRAPHQL_DATABASE_URL'
    );
    cy.get('[data-test="connect-database-btn"]').click();

    cy.visit('/data/manage/edit/dynamic-db-routing');
  });

  after(() => {
    cy.visit('/data/manage/', {
      onBeforeLoad(win) {
        cy.stub(win, 'prompt').returns('dynamic-db-routing');
      },
    });
    // click on the third remove button
    cy.get('[data-test="remove-dynamic-db-routing"]', {
      timeout: 10000,
    }).click();
  });

  xit('When users update the Dynamic DB Routing connection template, the metadata should be updated', () => {
    cy.log('**--- Click on Dynamic DB Connection Routing**');
    cy.contains('Dynamic DB Connection Routing', {
      timeout: 10000,
    }).click();

    cy.log('**--- Click on Database Tenancy**');
    cy.contains('Database Tenancy').click();

    cy.log('**--- Click on Add Connection**');
    cy.contains('Add Connection').click();

    cy.log('**--- Type in the Connection Name**');
    cy.get('[data-testid="name"]').type('test-connection');

    cy.log('**--- Type in the Connection String**');
    cy.get('[data-testid="configuration.connectionInfo.databaseUrl.url"]').type(
      'postgres://postgres:postgres@localhost:5433/postgres'
    );

    cy.log('**--- Click on Add Connection**');
    cy.get(
      '[data-analytics-name="data-tab-dynamic-db-routing-add-connection-submit"]'
    ).click();

    cy.log('**--- Wait for metadata update to finish **');
    cy.intercept('POST', '**/v1/metadata').as('metadataRequest');
    cy.wait('@metadataRequest');

    cy.log('**--- Click on Edit connection**');
    cy.contains('Edit Connection').click();

    cy.log('**--- Change the URL**');
    cy.get('[data-testid="configuration.connectionInfo.databaseUrl.url"]')
      .clear()
      .type('postgres://postgres:postgres@localhost:5432/postgres');

    cy.log('**--- Click Update Connection ');
    cy.contains('Update Connection').click();

    cy.log('**--- Wait for metadata update to finish **');
    cy.intercept('POST', '**/v1/metadata').as('metadataRequest2');
    cy.wait('@metadataRequest2');

    cy.log('**--- Click Update Connection Template**');
    cy.contains('Update Connection Template').click();

    cy.log('**--- Wait for metadata update to finish **');
    cy.intercept('POST', '**/v1/metadata').as('metadataRequest3');
    cy.wait('@metadataRequest3');

    readMetadata().then((md: { body: Metadata['metadata'] }) => {
      cy.wrap(
        (md.body.sources || []).find(
          source => source.name === 'dynamic-db-routing'
        )
      ).toMatchSnapshot({ name: 'Action metadata' });
    });

    cy.log('**--- Click on Delete Connection**');
    cy.contains('Remove').click();

    // wait to finish
    cy.wait('@metadataRequest');

    cy.log('**--- Click on disabled**');
    cy.contains('Disabled').click();

    cy.log('**--- Click on Update Connection Template **');
    cy.contains('Update Connection Template').click();
  });
});
