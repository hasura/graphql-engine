import { logMetadataRequests } from './utils/requests/logMetadataRequests';
import { openTelemetryMustNotExist } from './utils/testState/openTelemetryMustNotExist';

// NOTE: This test suite does not include cases for relationships, headers and the codegen part

// Why this test is skipped?
// - because OpenTelemetry is a "Pro Console" feature (it works with a EE Lite server)
// - at the moment, we do not have E2E tests for the Pro Console
// - anyway, from a server standpoint, an EE Lite server is available in CI and usable for the E2E tests
// but these tests are fundamental to locally test the OpenTelemetry feature and being sure it works
// properly
describe.skip('OpenTelemetry', () => {
  beforeEach(() => {
    openTelemetryMustNotExist();
    logMetadataRequests();

    cy.visit('/settings/opentelemetry');
  });

  afterEach(() => {
    openTelemetryMustNotExist();
  });

  it('When OpenTelemetry is set up, then everything should work', () => {
    cy.log('**--- STEP: Enable OpenTelemetry**');
    cy.findByLabelText('Status').click();

    cy.log('**--- STEP: Type the Endpoint**');
    cy.findByLabelText('Endpoint', { selector: 'input' }).type(
      'http://example.io'
    );

    cy.log('**--- STEP: Click the Submit button**');
    cy.findByRole('button', { name: 'Connect' }).click();

    cy.log('**--- STEP: Check the success notification**');
    cy.expectSuccessNotificationWithMessage(
      'Successfully updated the OpenTelemetry Configuration'
    );
  });
});
