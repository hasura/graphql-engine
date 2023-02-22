import {
  trackCustomEvent,
  programmaticallyTraceError,
} from '../../../Analytics';

import {
  parseUnexistingEnvVarSchemaError,
  parseHasuraEnvVarsNotAllowedError,
} from '../../../hasura-metadata-types';

import { useFireNotification } from '../../../../new-components/Notifications';

export function useOnSetOpenTelemetryError(
  fireNotification: ReturnType<typeof useFireNotification>['fireNotification']
) {
  return function onSetOpenTelemetryError(err: unknown) {
    // UNEXISTING ENV VAR ERROR
    const parseUnexistingEnvVarSchemaResult =
      parseUnexistingEnvVarSchemaError(err);
    if (parseUnexistingEnvVarSchemaResult.success) {
      fireNotification({
        type: 'error',
        title: 'Error!',
        message: parseUnexistingEnvVarSchemaResult.data.internal[0].reason,
      });

      trackCustomEvent(
        {
          location: 'OpenTelemetry',
          action: 'update OpenTelemetry',
          object: 'Unexisting env var error',
        },
        {
          severity: 'error',
        }
      );

      return;
    }

    // HASURA ENV VAR ERROR
    const parseHasuraEnvVarsNotAllowedResult =
      parseHasuraEnvVarsNotAllowedError(err);
    if (parseHasuraEnvVarsNotAllowedResult.success) {
      fireNotification({
        type: 'error',
        title: 'Error!',
        message: parseHasuraEnvVarsNotAllowedResult.data.error,
      });

      trackCustomEvent(
        {
          location: 'OpenTelemetry',
          action: 'update OpenTelemetry',
          object: 'Hasura env var error',
        },
        {
          severity: 'error',
        }
      );

      return;
    }

    // UNEXPECTED ERROR
    fireNotification({
      type: 'error',
      title: 'Error!',
      message: JSON.stringify(err),
    });

    trackCustomEvent(
      {
        location: 'OpenTelemetry',
        action: 'update OpenTelemetry',
        object: 'Unexpected error',
      },
      {
        severity: 'error',
      }
    );

    programmaticallyTraceError({
      error: 'OpenTelemetry set_opentelemetry_config error not parsed',
      cause: err instanceof Error ? err : undefined,
    });
  };
}
