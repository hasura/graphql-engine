import { testMode } from '../../../helpers/common';
import { setMetaData } from '../../validators/validators';

import { getIndexRoute } from '../../../helpers/dataHelpers';
import {
  checkCreateOneOffTriggerRoute,
  expandOneOffLogs,
  expandOneOffPendingEvent,
  expandOneOffProcessedEvent,
  scheduleOneoffEvent,
} from './spec';

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

export const runCreateOneOffTriggerTests = () => {
  describe('Create One-off Trigger', () => {
    it(
      'One-off trigger button opens the correct route',
      checkCreateOneOffTriggerRoute
    );
    it('schedule an event', scheduleOneoffEvent);
    it('will expand the pending events', expandOneOffPendingEvent);
    it('will expand the processed evenets', expandOneOffProcessedEvent);
    it('will exapnd the invocation logs', expandOneOffLogs);
  });
};

if (testMode !== 'cli') {
  setup();
  runCreateOneOffTriggerTests();
}
