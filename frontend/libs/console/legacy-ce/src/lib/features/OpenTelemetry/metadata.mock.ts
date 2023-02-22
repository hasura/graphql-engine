import produce from 'immer';

import type { MetadataReducer } from '../../mocks/actions';
import type { allowedMetadataTypes } from '../MetadataAPI';
import type { Metadata, SetOpenTelemetryQuery } from '../hasura-metadata-types';

export const openTelemetryInitialData: Partial<Metadata['metadata']> = {
  opentelemetry: undefined,
};

export const metadataHandlers: Partial<
  Record<allowedMetadataTypes, MetadataReducer>
> = {
  // ATTENTION: the server errors that the Console prevents are not handled here
  set_opentelemetry_config: (state, action) => {
    const newOpenTelemetry = action.args as SetOpenTelemetryQuery['args'];

    // TODO: manage error if needed

    return produce(state, draft => {
      draft.metadata.opentelemetry = newOpenTelemetry;
    });
  },
};
