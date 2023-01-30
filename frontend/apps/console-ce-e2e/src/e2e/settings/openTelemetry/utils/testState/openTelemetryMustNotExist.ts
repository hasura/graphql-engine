import { readMetadata } from '../services/readMetadata';
import { deleteOpenTelemetry } from '../services/deleteOpenTelemetry';

/**
 * Ensure the OpenTelemetry configuration does not exist.
 */
export function openTelemetryMustNotExist() {
  Cypress.log({ message: '**--- OpenTelemetry check: start**' });

  readMetadata().then(response => {
    const openTelemetryExists = !!response.body.opentelemetry;

    if (openTelemetryExists) {
      Cypress.log({ message: '**--- OpenTelemetry must be deleted**' });
      deleteOpenTelemetry();
    }
  });
}
