import produce from 'immer';

import type { MetadataReducer } from '@/mocks/actions';
import type { allowedMetadataTypes } from '@/features/MetadataAPI';
import type {
  Metadata,
  SetOpenTelemetryConfigQuery,
} from '@/features/hasura-metadata-types';
import { parseOpenTelemetry } from '@/features/hasura-metadata-types';

export const openTelemetryConfigInitialData: Partial<Metadata['metadata']> = {
  opentelemetry: undefined,
};

export const metadataHandlers: Partial<
  Record<allowedMetadataTypes, MetadataReducer>
> = {
  // ATTENTION: the server errors that the Console prevents are not handled here
  set_opentelemetry_config: (state, action) => {
    // TODO: strongly type it
    const newOpenTelemetryConfig =
      action.args as SetOpenTelemetryConfigQuery['args'];

    const result = parseOpenTelemetry(newOpenTelemetryConfig);
    if (!result.success) {
      throw result.error;
    }

    return produce(state, draft => {
      draft.metadata.opentelemetry = newOpenTelemetryConfig;
    });
  },
};
