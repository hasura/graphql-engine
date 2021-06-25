import { setMetaData } from '../../validators/validators';
import { getIndexRoute } from '../../../helpers/dataHelpers';
import {
  openManageDatabases,
  expandAddDatabaseForm,
  expandConnectionSettingsform,
  failsOnEmptyFormSubmission,
  addsNewPostgresDatabaseWithUrl,
  failDuplicateNameDb,
  addsNewPgDBWithConParams,
  addsNewPgDBWithEnvVar,
  deleteTestDBWithUrl,
  deleteTestDBWithConParams,
  deleteTestDDWithEnvVar,
} from './spec';
import { testMode } from '../../../helpers/common';

const setup = () => {
  describe('Setup route', () => {
    it('Visit the index route', () => {
      cy.visit(getIndexRoute()).then(setMetaData);
    });
  });
};

export const runManageDatabaseTests = () => {
  describe('Manage Data sources Tests', () => {
    it('Opens Manage Database route', openManageDatabases);
    it('Opens Add database form', expandAddDatabaseForm);
    it('Expands Connection settings form', expandConnectionSettingsform);
    it('Fails on empty form submission', failsOnEmptyFormSubmission);
    it(
      'Successfully creates a new postgres database with database url',
      addsNewPostgresDatabaseWithUrl
    );
    it(
      'Successfully creates a new postgres database with connection parameters',
      addsNewPgDBWithConParams
    );
    it(
      'Successfully creates a new postgres database with env variable ',
      addsNewPgDBWithEnvVar
    );
    it('Fails to connect db with duplicate name', failDuplicateNameDb);
    it('Successfully deletes test DB added with URL', deleteTestDBWithUrl);
    it(
      'Successfully deletes test DB added with connection params',
      deleteTestDBWithConParams
    );
    it(
      'Successfully deletes test DB added with env variable',
      deleteTestDDWithEnvVar
    );
  });
};

if (testMode !== 'cli') {
  setup();
  runManageDatabaseTests();
}
