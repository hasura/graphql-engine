/* eslint no-unused-vars: 0 */
/* eslint import/prefer-default-export: 0 */
import { testMode } from '../../../helpers/common';
import { setMetaData } from '../../validators/validators';

import {
  checkCreateTriggerRoute,
  failCTWithoutData,
  passCT2,
  failCTDuplicateTrigger,
  insertTableRow,
  deleteCTTestTrigger,
  deleteCTTestTable,
  passPTCreateTable,
  passCT1,
  createEtTransform,
  modifyEtTransform,
  deleteEtTransform,
} from './spec';
import { getIndexRoute } from '../../../helpers/dataHelpers';

const setup = () => {
  describe('Check Data Tab', () => {
    it('Clicking on Data tab opens the correct route', () => {
      // Visit the index route
      cy.visit(getIndexRoute());
      // Get and set validation metadata
      setMetaData();
    });
  });
};

export const runCreateTriggerTests = () => {
  describe('Create Trigger', () => {
    it('Create test table', passPTCreateTable);
    it(
      'Create trigger button opens the correct route',
      checkCreateTriggerRoute
    );
    it('Fails to create trigger without data', failCTWithoutData);
    it('Successfuly creates trigger with selected columns for update', passCT1);
    it('Successfuly creates trigger with all columns for update', passCT2);
    it('Fails to create duplicate trigger', failCTDuplicateTrigger);
    it('Insert a row and invoke trigger', insertTableRow);
    it("Delete's the test trigger", deleteCTTestTrigger);
    it('Create Event Trigger With Transform', createEtTransform);
    it('Update Event Trigger With Transform', modifyEtTransform);
    it('Delete Event Trigger With Transform', deleteEtTransform);
    it("Delete's the test table", deleteCTTestTable);
  });
};

if (testMode !== 'cli') {
  setup();
  runCreateTriggerTests();
}
