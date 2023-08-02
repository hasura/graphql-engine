import { z } from 'zod';
import produce from 'immer';

import type {
  SetOpenTelemetryQuery,
  hasuraEnvVarsNotAllowedSchema,
} from '@hasura/console-legacy-ce';
import { unexistingEnvVarSchema } from '@hasura/console-legacy-ce';

import { blockServerRequests } from './utils/requests/blockServerRequests';

import {
  stubInitialServerRequests,
  waitForInitialServerRequests,
} from './fixtures/initialRequests/stubInitialServerRequests';

import { export_metadata } from './fixtures/initialRequests/export_metadata';

// Why this test is skipped?
// - because OpenTelemetry is a "Pro Console" feature (it works with a EE Lite server)
// - at the moment, we do not have E2E tests for the Pro Console
// but these tests are fundamental to locally test the OpenTelemetry feature and being sure it works
// properly
describe.skip('OpenTelemetry', () => {
  beforeEach(() => {
    cy.log('**--- Start controlling the server**');
    blockServerRequests();
    stubInitialServerRequests();

    cy.log('**--- Load the Console**');
    cy.visit('/settings/opentelemetry');

    waitForInitialServerRequests();
  });

  it('When OpenTelemetry is set up, then everything should work', () => {
    cy.log('**--- STEP: Enable OpenTelemetry**');
    cy.findByLabelText('Status').click();

    cy.log('**--- STEP: Type the Endpoint**');
    cy.findByLabelText('Endpoint', { selector: 'input' }).type(
      'http://example.io'
    );

    cy.log(
      '**--- STEP: Intercept the set_opentelemetry_config request and the next export_metadata one**'
    );
    let openTelemetryFixture: SetOpenTelemetryQuery['args'] | undefined;
    cy.intercept('POST', 'http://localhost:8080/v1/metadata', req => {
      if (req.body.type === 'set_opentelemetry_config') {
        Cypress.log({
          message: '**--- STEP: Intercept the set_opentelemetry_config call**',
        });

        req.alias = 'set_opentelemetry_config';

        // Steal the server openTelemetry configuration from the one passed by the Console
        openTelemetryFixture = req.body.args;

        const fixture: SetOpenTelemetryQuery['successResponse'] = {
          message: 'success',
        };
        req.reply(fixture);
      }

      if (req.body.type === 'export_metadata') {
        Cypress.log({
          message: '**--- STEP: Intercept the export_metadata call**',
        });

        req.alias = 'export_metadata_with_opentelemetry';

        req.reply(
          // Use the openTelemetry configuration passed from the Console to the server to get back
          // a metadata that includes the same configuration
          produce(export_metadata, draft => {
            draft.metadata.opentelemetry = openTelemetryFixture;
          })
        );
      }
    });

    cy.log('**--- STEP: Click the Submit button**');
    // Why update and not connect?
    cy.findByRole('button', { name: 'Connect' }).click();

    cy.log('**--- STEP: Wait for the Console to save OpenTelemetry**');
    cy.wait('@set_opentelemetry_config');

    cy.log('**--- STEP: Wait for the Console to refetch metadata**');
    cy.wait('@export_metadata_with_opentelemetry');

    cy.log('**--- STEP: Check the success notification**');
    cy.expectSuccessNotificationWithMessage(
      'Successfully updated the OpenTelemetry Configuration'
    );
  });

  it('When an unexisting env var is added, then the user should be prompted about it', () => {
    cy.log(
      '**--- STEP: Intercept the set_opentelemetry_config request and return the error**'
    );
    cy.intercept('POST', 'http://localhost:8080/v1/metadata', req => {
      if (req.body.type === 'set_opentelemetry_config') {
        Cypress.log({
          message: '**--- STEP: Intercept the set_opentelemetry_config call**',
        });

        req.alias = 'set_opentelemetry_config';

        const fixture: z.infer<typeof unexistingEnvVarSchema> = {
          // That's the only part of thr error the Console cares about
          internal: [
            {
              reason: `Inconsistent object: environment variable 'foo' not set`,
            },
          ],
        };
        req.reply({
          statusCode: 400,
          body: fixture,
        });
      }
    });

    cy.log('**--- STEP: Click the Submit button**');
    cy.findByRole('button', { name: 'Connect' }).click();

    cy.log('**--- STEP: Wait for the Console to save OpenTelemetry**');
    cy.wait('@set_opentelemetry_config');

    cy.log('**--- STEP: Check the error notification**');
    cy.expectErrorNotificationWithMessage(
      `Inconsistent object: environment variable 'foo' not set`
    );
  });

  it('When an HASURA_GRAPHQL_ env var is added, then the user should be prompted about it', () => {
    cy.log(
      '**--- STEP: Intercept the set_opentelemetry_config request and return the error**'
    );
    cy.intercept('POST', 'http://localhost:8080/v1/metadata', req => {
      if (req.body.type === 'set_opentelemetry_config') {
        Cypress.log({
          message: '**--- STEP: Intercept the set_opentelemetry_config call**',
        });

        req.alias = 'set_opentelemetry_config';

        const fixture: z.infer<typeof hasuraEnvVarsNotAllowedSchema> = {
          // That's the only part of thr error the Console cares about

          error:
            'env variables starting with "HASURA_GRAPHQL_" are not allowed in value_from_env: HASURA_GRAPHQL_ENABLED_APIS',
        };
        req.reply({
          statusCode: 400,
          body: fixture,
        });
      }
    });

    cy.log('**--- STEP: Click the Submit button**');
    cy.findByRole('button', { name: 'Connect' }).click();

    cy.log('**--- STEP: Wait for the Console to save OpenTelemetry**');
    cy.wait('@set_opentelemetry_config');

    cy.log('**--- STEP: Check the error notification**');
    cy.expectErrorNotificationWithMessage(
      `env variables starting with "HASURA_GRAPHQL_" are not allowed in value_from_env: HASURA_GRAPHQL_ENABLED_APIS`
    );
  });

  it('When an unexpected error is returned from the server, then the user should be prompted about it', () => {
    cy.log(
      '**--- STEP: Intercept the set_opentelemetry_config request and return the error**'
    );
    cy.intercept('POST', 'http://localhost:8080/v1/metadata', req => {
      if (req.body.type === 'set_opentelemetry_config') {
        Cypress.log({
          message: '**--- STEP: Intercept the set_opentelemetry_config call**',
        });

        req.alias = 'set_opentelemetry_config';

        const fixture = {
          unmanagedError: 'An error the Console does not manage',
        };
        req.reply({
          statusCode: 400,
          body: fixture,
        });
      }
    });

    cy.log('**--- STEP: Click the Submit button**');
    cy.findByRole('button', { name: 'Connect' }).click();

    cy.log('**--- STEP: Wait for the Console to save OpenTelemetry**');
    cy.wait('@set_opentelemetry_config');

    cy.log('**--- STEP: Check the error notification**');
    cy.expectErrorNotificationWithMessage(
      `{"unmanagedError":"An error the Console does not manage"}`
    );
  });
});
