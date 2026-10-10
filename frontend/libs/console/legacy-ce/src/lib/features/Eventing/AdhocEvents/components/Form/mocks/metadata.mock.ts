import { AllowedMetadataTypes } from '@hasura/shared/types';

// scheduled events doesn't modify the metadata
export const metadataHandlers: Partial<Record<AllowedMetadataTypes, any>> = {
  create_scheduled_event: (state) => {
    return state;
  },
};
