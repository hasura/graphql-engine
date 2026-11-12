import { useQueries } from '@tanstack/react-query';
import { useMetadata } from '@hasura/metadata/api';
import { getDatabaseMethods } from '@hasura/metadata/data-source';
import { isNativeDriver } from '@hasura/metadata/helpers';
import { useAppContext, useAuthContext } from '@hasura/shared/context';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { hasDataAccess } from '../../../../shared/auth/privileges';
import { getDataConnectorScalarReplacements } from './utils';

// Loads the capabilities of the data connectors of the metadata sources, to
// replace their scalars in the types of derived actions.
export const useDataConnectorScalarReplacements = () => {
  const { endpoints } = useAppContext();
  const { privileges } = useAuthContext();
  const fetchJson = useAuthFetchJson();

  // the metadata API is admin-only
  const { data: dataConnectorKinds = [] } = useMetadata(
    (m) => [
      ...new Set(
        m.metadata.sources
          .map((source) => source.kind)
          .filter((kind) => !isNativeDriver(kind)),
      ),
    ],
    { enabled: hasDataAccess(privileges) },
  );

  return useQueries({
    queries: dataConnectorKinds.map((kind) => ({
      queryKey: [kind, 'source-kind-capabilities'],
      queryFn: () =>
        getDatabaseMethods(kind).introspection.getDriverCapabilities({
          endpoints,
          fetchJson,
          driver: kind,
        }),
      staleTime: Infinity,
      refetchOnWindowFocus: false,
    })),
    combine: (results) =>
      getDataConnectorScalarReplacements(results.map((r) => r.data)),
  });
};
