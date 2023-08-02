import type { SetOpenTelemetryQuery } from '../../../hasura-metadata-types';

import { useMetadataMigration } from '../../../MetadataAPI';

import { useFireNotification } from '../../../../new-components/Notifications';

import type { FormValues } from '../../OpenTelemetry/components/Form/schema';

import { formValuesToOpenTelemetry } from '../utils/openTelemetryToFormValues';
import { useOnSetOpenTelemetryError } from './useOnSetOpenTelemetryError';
import { useMetadata } from '../../../hasura-metadata-api';

type QueryArgs = SetOpenTelemetryQuery['args'];

function errorTransform(error: unknown) {
  return error;
}

/**
 * Allow updating the OpenTelemetry configuration.
 */
export function useSetOpenTelemetry() {
  const { mutate, ...rest } = useMetadataMigration({ errorTransform });
  const { data: version } = useMetadata(m => m.resource_version);

  const { fireNotification } = useFireNotification();

  const onSetOpenTelemetryError = useOnSetOpenTelemetryError(fireNotification);

  const setOpenTelemetry = (formValues: FormValues) => {
    const args: QueryArgs = formValuesToOpenTelemetry(formValues);

    // Please note: not checking if the component is still mounted or not is made on purpose because
    // the callbacks do not direct mutate any component state.
    return new Promise<void>(resolve => {
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

            fireNotification({
              title: 'Success!',
              message: 'Successfully updated the OpenTelemetry Configuration',
              type: 'success',
            });
          },

          onError: err => {
            // The promise is used by Rect hook form to stop show the loading spinner but React hook
            // form must not handle errors.
            resolve();

            onSetOpenTelemetryError(err);
          },
        }
      );
    });
  };

  return {
    setOpenTelemetry,
    ...rest,
  };
}
