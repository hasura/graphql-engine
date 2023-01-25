import {
  getElementFromAlias,
  getTableName,
  getTriggerName,
  getWebhookURL,
  getNoOfRetries,
  getIntervalSeconds,
  getTimeoutSeconds,
  baseUrl,
} from '../../../helpers/eventHelpers';
import { validateCTrigger, ResultType } from '../../validators/validators';

const EVENT_TRIGGER_INDEX_ROUTE = '/events/data';

export const checkCreateOneOffTriggerRoute = () => {
  //    Click on the one-off scheduled events
  cy.visit(`${EVENT_TRIGGER_INDEX_ROUTE}/manage`);
  cy.wait(4000);
  cy.visit(EVENT_TRIGGER_INDEX_ROUTE);
  cy.wait(15000);
  cy.get(getElementFromAlias('one-off-trigger')).click();
  //   Match the URL
  cy.url().should('eq', `${baseUrl}/events/one-off-scheduled-events/info`);
};

export const scheduleOneoffEvent = () => {
  // click on the schedule event tab
  cy.visit('/events/one-off-scheduled-events/add');
  cy.url().should('eq', `${baseUrl}/events/one-off-scheduled-events/add`);
  // type webhook url
  cy.get('[data-testid=webhook]').type(getWebhookURL());
  // open retry configuration accordion
  cy.get('[data-testid=retry-configuration] > div > button').click();
  // type retry configuration
  cy.get('[data-testid=num_retries]').clear().type(getNoOfRetries());
  // type interval seconds
  cy.get('[data-testid=retry_interval_seconds]')
    .clear()
    .type(getIntervalSeconds());
  // type payload
  cy.get('textarea').clear({ force: true }).type('{}', { force: true });
  // type timeout seconds
  cy.get('[data-testid=timeout_seconds]').clear().type(getTimeoutSeconds());

  //  Click on create
  cy.get('[data-testid=create-scheduled-event]').click();
  cy.wait(10000);
  //  Check if the trigger got created and navigated to processed events page
  cy.url().should('eq', `${baseUrl}/events/one-off-scheduled-events/pending`);
  validateCTrigger(
    getTriggerName(0),
    getTableName(0),
    'public',
    ResultType.SUCCESS
  );
};

export const expandOneOffPendingEvent = () => {
  // expand button
  cy.get(getElementFromAlias('expand-event')).click();
  cy.wait(4000);
  // expand recent invocation
  cy.get(getElementFromAlias('expand-event')).click();
  cy.wait(4000);
  // collaspe
  cy.get(getElementFromAlias('collapse-event')).first().click();
  cy.wait(4000);
};

export const expandOneOffProcessedEvent = () => {
  // processed events tab
  cy.get(
    getElementFromAlias('adhoc-events-container-tabs-events-processed')
  ).click();
  cy.url().should('eq', `${baseUrl}/events/one-off-scheduled-events/processed`);
  // expand processed event
  cy.get(getElementFromAlias('expand-event')).first().click();
  cy.wait(4000);
  // expand recent invocation
  cy.get(getElementFromAlias('expand-event')).first().click();
  cy.wait(4000);
  // collaspe
  cy.get(getElementFromAlias('collapse-event')).first().click();
  cy.wait(4000);
};

export const expandOneOffLogs = () => {
  // invocation logs tab
  cy.get(
    getElementFromAlias('adhoc-events-container-tabs-events-logs')
  ).click();
  cy.url().should('eq', `${baseUrl}/events/one-off-scheduled-events/logs`);
  // expand logs
  cy.get(getElementFromAlias('expand-event')).first().click();
  cy.wait(4000);
  // expand recent invocation
  cy.get(getElementFromAlias('collapse-event')).click();
  cy.wait(4000);
};
