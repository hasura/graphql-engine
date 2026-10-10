import { useInconsistentMetadata } from './useInconsistentMetadata';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useInvalidateMetadata } from './useInvalidateMetadata';
import {
  getRemoteSchemaNameFromInconsistentObjects,
  getSourceFromInconsistentObjects,
} from '../../utils';
import { useAppContext } from '@hasura/shared/context';
import type { InconsistentObject } from '@hasura/shared/types';

const getReloadMetadataQuery = (
  shouldReloadRemoteSchemas: boolean | string[],
  shouldReloadSources?: boolean | string[],
) => ({
  type: 'reload_metadata',
  args: {
    reload_sources: shouldReloadSources ?? [],
    reload_remote_schemas: shouldReloadRemoteSchemas ?? [],
  },
});

export const useReloadMetadata = () => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const { data: inconsistentMetadata, isLoading } = useInconsistentMetadata();
  const invalidate = useInvalidateMetadata();

  const reloadMetadata = ({
    shouldReloadAllSources = false,
    shouldReloadRemoteSchemas = false,
  }: {
    shouldReloadRemoteSchemas?: boolean;
    shouldReloadAllSources?: boolean;
  }) => {
    const inconsistentSources = inconsistentMetadata?.inconsistent_objects
      .length
      ? getSourceFromInconsistentObjects(
          inconsistentMetadata?.inconsistent_objects,
        )
      : [];
    const inconsistentRemoteSchemas = inconsistentMetadata?.inconsistent_objects
      .length
      ? getRemoteSchemaNameFromInconsistentObjects(
          inconsistentMetadata.inconsistent_objects,
        )
      : [];

    let reloadSources: string[] | boolean = [];
    if (shouldReloadAllSources) {
      reloadSources = true;
    } else if (inconsistentSources.length) {
      reloadSources = inconsistentSources;
    }

    let reloadRemoteSchemas: string[] | boolean = [];
    if (shouldReloadRemoteSchemas) {
      reloadRemoteSchemas = true;
    } else if (inconsistentRemoteSchemas.length) {
      reloadRemoteSchemas = inconsistentRemoteSchemas;
    }

    const loadQuery = getReloadMetadataQuery(
      reloadRemoteSchemas,
      reloadSources,
    );

    return fetchJson<{
      is_consistent: boolean;
      // HGE only includes `inconsistent_objects` when there are any (i.e. when
      // `is_consistent` is false), so it is optional here.
      inconsistent_objects?: InconsistentObject[];
    }>(endpoints.metadata, {
      method: 'POST',
      body: JSON.stringify(loadQuery),
    }).then((result) => {
      invalidate();
      return result.is_consistent;
    });
  };

  return {
    reloadMetadata,
    inconsistentMetadata,
    isLoading,
  };
};

export type UseReloadMetadata = ReturnType<typeof useReloadMetadata>;
