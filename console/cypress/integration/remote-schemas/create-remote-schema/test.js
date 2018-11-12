/* eslint no-unused-vars: 0 */
/* eslint import/prefer-default-export: 0 */
import { testMode } from '../../../helpers/common';
import { setMetaData } from '../../validators/validators';

import {
  checkCreateRemoteSchemaRoute,
  failRSWithInvalidRemoteUrl,
  createSimpleRemoteSchema,
  deleteSimpleRemoteSchema,
  deleteSimpleRemoteSchemaFailUserConfirmationError,
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
    it(
      'Delete simple remote schema fail due to user confirmation error',
      deleteSimpleRemoteSchemaFailUserConfirmationError
    );
    it('Delete simple remote schema', deleteSimpleRemoteSchema);
  });
};

if (testMode !== 'cli') {
  setup();
  runCreateRemoteSchemaTableTests();
}
