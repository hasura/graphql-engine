/* eslint no-unused-vars: 0 */
/* eslint import/prefer-default-export: 0 */
import { testMode } from '../../../helpers/common';
import { setMetaData } from '../../validators/validators';

import {
  checkCreateRemoteSchemaRoute,
  failRSWithInvalidRemoteUrl,
  createSimpleRemoteSchema,
  failRSDuplicateSchemaName,
  failRSDuplicateSchemaNodes,
  deleteSimpleRemoteSchema,
  deleteSimpleRemoteSchemaFailUserConfirmationError,
  failWithRemoteSchemaEnvUrl,
  failWithRemoteSchemaEnvHeader,
  passWithRemoteSchemaHeader,
  passWithEditRemoteSchema,
  deleteRemoteSchema,
} from './spec';

const setup = () => {
  describe('Setup route', () => {
    it('Visit the index route', () => {
      // Visit the index route
      cy.visit('/remote-schemas/manage/schemas');
      cy.wait(7000);
      // Get and set validation metadata
      setMetaData();
    });
  });
};

export const runCreateRemoteSchemaTableTests = () => {
  describe('Create Remote Schema', () => {
    it(
      'Create table button opens the correct route',
      checkCreateRemoteSchemaRoute
    );
    it(
      'Fails to create remote schema without name',
      failRSWithInvalidRemoteUrl
    );
    it('Create a simple remote schema', createSimpleRemoteSchema);
    it('Fails to add remote schema with same name', failRSDuplicateSchemaName);
    it(
      'Fails to add remote schema which is already added',
      failRSDuplicateSchemaNodes
    );
    it(
      'Delete simple remote schema fail due to user confirmation error',
      deleteSimpleRemoteSchemaFailUserConfirmationError
    );
    it('Delete simple remote schema', deleteSimpleRemoteSchema);
    it('Create remote schema with url from env', failWithRemoteSchemaEnvUrl);
    it(
      'Create remote schema with headers from env',
      failWithRemoteSchemaEnvHeader
    );
    it('Create remote schema with headers', passWithRemoteSchemaHeader);
    it('Edit remote schema with headers', passWithEditRemoteSchema);
    it('Delete remote schema with headers', deleteRemoteSchema);
  });
};

if (testMode !== 'cli') {
  setup();
  runCreateRemoteSchemaTableTests();
}
