import { useInconsistentMetadata } from '../../hasura-metadata-api';

export const useInconsistentSources = () => {
  return useInconsistentMetadata(m => {
    return m.inconsistent_objects.filter(
      inconsistentObject => inconsistentObject.type === 'source'
    );
  });
};
