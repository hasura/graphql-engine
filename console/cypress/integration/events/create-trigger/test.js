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

const testPrefix = 'create-trigger';

const makeAssertion = (assertion, specFunc) => {
  it(`${testPrefix} : ${assertion}`, specFunc);
};

export const runCreateTriggerTests = () => {
  describe('Create Trigger', () => {
    makeAssertion('Create table to use in triggers', passPTCreateTable);
    makeAssertion('Visit events manage page', visitEventsManagePage);
    makeAssertion(
      'Create trigger button opens the correct route',
      checkCreateTriggerRoute
    );
    makeAssertion('Fails to create trigger without data', failCTWithoutData);
    makeAssertion('Successfuly creates trigger', passCT);
    makeAssertion('Fails to create duplicate trigger', failCTDuplicateTrigger);
    makeAssertion('Insert a row and invoke trigger', insertTableRow);
    makeAssertion('Delete off the test trigger', deleteCTTestTrigger);
    makeAssertion('Delete off the test table', deleteCTTestTable);
  });
};

if (testMode !== 'cli') {
  setup();
  runCreateTriggerTests();
}
