import type { SetOpenTelemetryQuery } from '@hasura/shared/types';
import { useMetadataMigration, useMetadata } from '@hasura/metadata/api';
import type { FormValues } from '../../OpenTelemetry/components/Form/schema';
import { formValuesToOpenTelemetry } from '../utils/openTelemetryToFormValues';

import {
  trackCustomEvent,
  programmaticallyTraceError,
} from '@hasura/shared/analytics';
import {
  parseUnexistingEnvVarSchemaError,
  parseHasuraEnvVarsNotAllowedError,
} from '@hasura/metadata/helpers';
import { hasuraToast } from '@hasura/shared/ui';

type QueryArgs = SetOpenTelemetryQuery['args'];

/**
 * Allow updating the OpenTelemetry configuration.
 */
export function useSetOpenTelemetry() {
  const { mutate, ...rest } = useMetadataMigration();
  const { data: version } = useMetadata((m) => m.resource_version);

  const onSetOpenTelemetryError = (err: unknown) => {
    // UNEXISTING ENV VAR ERROR
    const parseUnexistingEnvVarSchemaResult =
      parseUnexistingEnvVarSchemaError(err);
    if (parseUnexistingEnvVarSchemaResult.success) {
      hasuraToast({
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
        },
      );

      return;
    }

    // HASURA ENV VAR ERROR
    const parseHasuraEnvVarsNotAllowedResult =
      parseHasuraEnvVarsNotAllowedError(err);
    if (parseHasuraEnvVarsNotAllowedResult.success) {
      hasuraToast({
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
        },
      );

      return;
    }

    // UNEXPECTED ERROR
    hasuraToast({
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
      },
    );

    programmaticallyTraceError({
      error: 'OpenTelemetry set_opentelemetry_config error not parsed',
      cause: err instanceof Error ? err : undefined,
    });
  };

  const setOpenTelemetry = (formValues: FormValues) => {
    const args: QueryArgs = formValuesToOpenTelemetry(formValues);

    // Please note: not checking if the component is still mounted or not is made on purpose because
    // the callbacks do not direct mutate any component state.
    return new Promise<void>((resolve) => {
      mutate(
        {
          query: {
            type: 'set_opentelemetry_config',
            args,
            resource_version: version,
          },
        },
        {
          onSuccess: () => {
            resolve();

            hasuraToast({
              title: 'Success!',
              message: 'Successfully updated the OpenTelemetry Configuration',
              type: 'success',
            });
          },

          onError: (err) => {
            // The promise is used by Rect hook form to stop show the loading spinner but React hook
            // form must not handle errors.
            resolve();

            onSetOpenTelemetryError(err);
          },
        },
      );
    });
  };

  return {
    setOpenTelemetry,
    ...rest,
  };
}
