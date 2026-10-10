import { getDatabaseMethods } from '@hasura/metadata/data-source';
import { useCallback } from 'react';
import { useNavigate } from 'react-router';
import { useMetadata } from '@hasura/metadata/api';
import { useAuthFetchJson, useIsUnmounted } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { dataRoutes } from '@hasura/shared/utils';

export const useGDCTreeItemClick = () => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const { data: metadataData } = useMetadata();
  const isUnmounted = useIsUnmounted();
  const navigate = useNavigate();

  const handleClick = useCallback(
    async (value) => {
      if (!value.length || !metadataData) return;

      if (isUnmounted()) return;

      const { database, ...rest } = JSON.parse(value[0]);
      const metadataSource = metadataData.metadata.sources.find(
        (source) => source.name === database,
      );

      if (!metadataSource)
        throw Error('useGDCTreeClick: source was not found in metadata');

      const databaseMethods = getDatabaseMethods(metadataSource.kind);

      // The capabilities result is not consumed here, but the call performs the
      // driver introspection request (side effect) and must still run/await.
      await databaseMethods.introspection.getDriverCapabilities({
        endpoints,
        fetchJson,
        driver: metadataSource.kind,
      });

      /**
       * Handling click for GDC DBs
       */
      const isTableClicked = Object.keys(rest?.table || {}).length !== 0;
      const isFunctionClicked = Object.keys(rest?.function || {}).length !== 0;
      if (isTableClicked) {
        navigate(dataRoutes.manageTable(database, rest.table));
      } else if (isFunctionClicked) {
        navigate(dataRoutes.manageFunction(database, rest.function));
      } else {
        navigate(dataRoutes.manageDatabaseSource(database));
      }
    },
    [metadataData, isUnmounted],
  );

  return { handleClick };
};
