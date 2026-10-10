import type {
  Metadata,
  SetOpenTelemetryQuery,
  SingleMetadataTypes,
} from '@hasura/shared/types';

export const openTelemetryInitialData: Partial<Metadata['metadata']> = {
  opentelemetry: undefined,
};

export const openTelemetryHandlers: Partial<Record<SingleMetadataTypes, any>> =
  {
    // ATTENTION: the server errors that the Console prevents are not handled here
    set_opentelemetry_config: (state, action) => {
      const newOpenTelemetry = action.args as SetOpenTelemetryQuery['args'];

      // TODO: manage error if needed

      return {
        ...state,
        metadata: {
          ...state.metadata,
          opentelemetry: newOpenTelemetry,
        },
      };
    },
  };
