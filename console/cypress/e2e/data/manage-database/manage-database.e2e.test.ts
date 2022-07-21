import { baseUrl, testMode } from '../../../helpers/common';
import {
  navigateAndOpenConnectDatabasesForm,
  navigateToManageDatabases,
  removeDBFromList,
  submitConnectDBForm,
} from './common.spec';
import {
  createDB,
  fillDetailsForPgConnParamsForm,
  fillDetailsForPgDbUrlForm,
  fillDetailsForPgEnvVarForm,
  removeDB,
} from './postgres.spec';

const connectPgDatabaseFormTests = () => {
  describe('Add a database via connect form', () => {
    describe('can successfully add', () => {
      describe('a postgres database', () => {
        it('using a connection string', () => {
          cy.log('**------------------------------**');
          cy.log('**--- Add postgres DB via connection string**');
          cy.log('**------------------------------**');

          navigateAndOpenConnectDatabasesForm();
          fillDetailsForPgDbUrlForm('postgres_db_with_url');
          submitConnectDBForm();

          cy.log('**--- Notifies that DB is being added');
          cy.expectSuccessNotificationWithTitle('Adding data source...');

          cy.log('**--- Redirects to Data Manager page');
          cy.getBySel('manage-database-section').within(() => {
            cy.findByText('Data Manager');
          });
          cy.url().should('eq', `${baseUrl}/data/manage`);

          cy.log('**--- has postgres_db_with_url on manage page');
          cy.findByText('postgres_db_with_url');

          cy.log('**--- has success notification displayed');
          cy.expectSuccessNotificationWithTitle(
            'Data source added successfully!'
          );

          cy.log('**--- Remove database**');
          removeDB('postgres_db_with_url');
        });

        it('using connection parameters', () => {
          cy.log('**------------------------------**');
          cy.log('**--- Add postgres DB via connection parameters**');
          cy.log('**------------------------------**');

          navigateAndOpenConnectDatabasesForm();
          fillDetailsForPgConnParamsForm('postgres_db_with_conn_param');
          submitConnectDBForm();

          cy.log('**--- notifies that db is being added');
          cy.expectSuccessNotificationWithTitle('Adding data source...');

          cy.log('**--- redirects to Data Manager page');
          cy.getBySel('manage-database-section').within(() => {
            cy.findByText('Data Manager');
          });
          cy.url().should('eq', `${baseUrl}/data/manage`);

          cy.log('**--- has postgres_db_with_conn_param on manage page');
          cy.findByText('postgres_db_with_conn_param');

          cy.log('**--- has success notification displayed');
          cy.expectSuccessNotificationWithTitle(
            'Data source added successfully!'
          );

          cy.log('**--- Remove database**');
          removeDB('postgres_db_with_conn_param');
        });

        it('using environment variables', () => {
          cy.log('**------------------------------**');
          cy.log('**--- Add postgres DB via env vars**');
          cy.log('**------------------------------**');

          navigateAndOpenConnectDatabasesForm();
          fillDetailsForPgEnvVarForm('postgres_db_with_env_var');
          submitConnectDBForm();

          cy.log('**--- notifies that db is being added');
          cy.expectSuccessNotificationWithTitle('Adding data source...');

          cy.log('**--- redirects to Data Manager page');
          cy.getBySel('manage-database-section').within(() => {
            cy.findByText('Data Manager');
          });
          cy.url().should('eq', `${baseUrl}/data/manage`);

          cy.log('**--- has postgres_db_with_env_var on manage page');
          cy.findByText('postgres_db_with_env_var');

          cy.log('**--- has success notification displayed');
          cy.expectSuccessNotificationWithTitle(
            'Data source added successfully!'
          );

          cy.log('**--- Remove database**');
          removeDB('postgres_db_with_env_var');
        });
      });
    });

    describe('fails on submitting', () => {
      describe('an empty form', () => {
        it('submit with no inputs filled in', () => {
          navigateAndOpenConnectDatabasesForm();
          cy.getBySel('connect-database-btn').click();
          cy.expectErrorNotification();
        });
      });

      it('with a duplicate database name', () => {
        navigateAndOpenConnectDatabasesForm();
        createDB('test');
        fillDetailsForPgDbUrlForm('test');
        submitConnectDBForm();

        cy.log('**--- verify error notification');
        cy.expectErrorNotificationWithTitle('Adding data source failed');

        cy.log('**--- Remove database**');
        removeDB('test');
      });
    });
  });
};

const manageDatabasesPageTests = () => {
  describe('Connected Databases list page', () => {
    it('can successfully remove db', () => {
      cy.log('**--- Create database**');
      createDB('db_for_removal');
      navigateToManageDatabases();

      cy.log('**--- use the remove button');
      removeDBFromList('db_for_removal');

      cy.log('**--- verify success notification');
      cy.expectSuccessNotificationWithTitle('Data source removed successfully');
    });
  });
};

if (testMode !== 'cli') {
  connectPgDatabaseFormTests();
  manageDatabasesPageTests();
}
