import { baseUrl, testMode } from '../../../helpers/common';
import {
  driverSpecType,
  expectNotif,
  navigateAndOpenConnectDatabasesForm,
  navigateToManageDatabases,
  removeDBFromList,
  submitConnectDBForm,
  verifyUrl,
} from './common.spec';
import { postgres } from './postgres.spec';
import { mssql } from './mssql.spec';

const connectDatabaseFormTests = () => {
  describe('Adding a database via connect form', () => {
    /* 
      Check if the connect DB form is able to create different types of databases
    */
    const drivers: driverSpecType[] = [postgres, mssql];

    describe('can successfully add', () => {
      drivers.forEach(driver => {
        describe(`a ${driver.name} database`, () => {
          const connectOptions: Record<string, any> = {
            with_url: {
              test: driver.tests.fillDetailsForDbUrlForm,
              description: 'using a connection string',
            },
            with_conn_param: {
              test: driver.tests.fillDetailsForConnParamsForm,
              description: 'using connection parameters',
            },
            with_env_var: {
              test: driver.tests.fillDetailsForEnvVarForm,
              description: 'using a ENV variable',
            },
          };

          Object.keys(connectOptions).forEach(key => {
            const option = connectOptions[key].test;
            if (option) {
              describe(connectOptions[key].description, () => {
                before(() => {
                  navigateAndOpenConnectDatabasesForm();
                  option(`${driver.name}_db_${key}`);
                  submitConnectDBForm();
                });

                it('verify success notification and redirect uri', () => {
                  expectNotif('success', {
                    title: 'Data source added successfully!',
                  });
                  verifyUrl(`${baseUrl}/data/manage`);
                });

                // cleanup
                after(() => {
                  driver.helpers.removeDB(`${driver.name}_db_${key}`);
                });
              });
            }
          });
        });
      });
    });

    /* 
      Check if the connect DB form's submit action results in an error for the following cases - 
      1. an empty from
      2. with a duplicate database name
    */
    describe('fails on submitting', () => {
      describe('an empty from', () => {
        it('submit with no inputs filled in', () => {
          navigateAndOpenConnectDatabasesForm();
          cy.getBySel('connect-database-btn').click();
          cy.get('.notification-error').should('be.visible');
        });
      });

      describe('with a duplicate database name', () => {
        // Add a db named "test" using the postgres helper and use the connect form to create a db with the same name "test"
        before(() => {
          navigateAndOpenConnectDatabasesForm();
          postgres.helpers.createDB('test');
          if (postgres.tests.fillDetailsForDbUrlForm)
            postgres.tests.fillDetailsForDbUrlForm('test');
          submitConnectDBForm();
        });

        // it should return an error notification
        it('verify error notification', () => {
          expectNotif('error', {
            title: 'Adding data source failed',
          });
        });

        // cleanup
        after(() => {
          postgres.helpers.removeDB('test');
        });
      });
    });
  });
};

const manageDatabasesPageTests = () => {
  describe('Connected Databases list page', () => {
    describe('can successfully remove db', () => {
      before(() => {
        postgres.helpers.createDB('db_for_removal');
        navigateToManageDatabases();
      });

      it('use the remove button', () => {
        removeDBFromList('db_for_removal');
      });

      it('verify success notification', () => {
        expectNotif('success', {
          title: 'Data source removed successfully',
        });
      });
    });
  });
};

if (testMode !== 'cli') {
  connectDatabaseFormTests();
  manageDatabasesPageTests();
}
