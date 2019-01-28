/* eslint no-unused-vars: 0 */
/* eslint import/prefer-default-export: 0 */
import { testMode } from '../../../helpers/common';
import { setMetaData } from '../../validators/validators';

import {
  passPTCreateTable,
  visitEventsManagePage,
  checkCreateTriggerRoute,
  failCTWithoutData,
  passCT,
  failCTDuplicateTrigger,
  failAddExistingTrigger,
  insertTableRow,
  deleteCTTestTrigger,
  deleteCTTestTable,
} from './spec';

const setup = () => {
  describe('Check Data Tab', () => {
    it('Clicking on Data tab opens the correct route', () => {
      // Visit the index route
      cy.visit('/data/schema/public');
      cy.wait(7000);
      // Get and set validation metadata
      setMetaData();
    });
  });
};

export const runCreateTriggerTests = () => {
  describe('Create Trigger', () => {
    it('Create table to use in triggers', passPTCreateTable);
    it('Visit events manage page', visitEventsManagePage);
    it(
      'Create trigger button opens the correct route',
      checkCreateTriggerRoute
    );
    it('Fails to create trigger without data', failCTWithoutData);
    it('Successfuly creates trigger', passCT);
    it('Fails to create duplicate trigger', failCTDuplicateTrigger);
    it('Insert a row and invoke trigger', insertTableRow);
    it('Delete off the test trigger', deleteCTTestTrigger);
    it('Delete off the test table', deleteCTTestTable);
  });
};

if (testMode !== 'cli') {
  setup();
  runCreateTriggerTests();
}
