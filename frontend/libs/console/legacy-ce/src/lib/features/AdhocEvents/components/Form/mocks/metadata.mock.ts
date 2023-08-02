import { allowedMetadataTypes } from '../../../../MetadataAPI';
import { MetadataReducer } from '../../../../../mocks/actions';

// scheduled events doesn't modify the metadata
export const metadataHandlers: Partial<
  Record<allowedMetadataTypes, MetadataReducer>
> = {
  create_scheduled_event: state => {
    return state;
  },
};
