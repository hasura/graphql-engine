import { createSelector } from 'reselect';

import { HasuraMetadataV2 } from '../../../metadata/types';
import { ReduxState } from '../../../types';
import { parseCustomTypes } from '../../../shared/utils/hasuraCustomTypeUtils';

const metadataSelector = (state: ReduxState): HasuraMetadataV2 | null => {
  return state.metadata.metadataObject;
};

export const actionsSelector = createSelector(metadataSelector, metadata => {
  if (!metadata) return [];

  return (
    metadata.actions?.map(action => ({
      ...action,
      definition: {
        ...action.definition,
        headers: action.definition.headers || [],
      },
      permissions: action.permissions || [],
    })) || []
  );
});

export const customTypesSelector = createSelector(
  metadataSelector,
  metadata => {
    if (!metadata) return [];

    return parseCustomTypes(metadata.custom_types);
  }
);
