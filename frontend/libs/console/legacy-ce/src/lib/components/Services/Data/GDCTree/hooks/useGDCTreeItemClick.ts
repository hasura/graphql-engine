import {
  DataSource,
  Feature,
  exportMetadata,
} from '../../../../../features/DataSource';
import { useHttpClient } from '../../../../../features/Network';
import { Dispatch } from '../../../../../types';
import { useCallback } from 'react';
import { getRoute } from '../../../../../utils/getDataRoute';
import { useIsUnmounted } from '../../Common/tsUtils';
import _push from '../../push';
import { defaultTab } from '../utils';

export const useGDCTreeItemClick = (dispatch: Dispatch) => {
  const httpClient = useHttpClient();
  const isUnmounted = useIsUnmounted();

  const handleClick = useCallback(
    async value => {
      if (!value.length) return;

      if (isUnmounted()) return;

      const { metadata } = await exportMetadata({ httpClient });
      const { database, ...rest } = JSON.parse(value[0]);
      const metadataSource = metadata.sources.find(
        source => source.name === database
      );

      if (!metadataSource)
        throw Error('useGDCTreeClick: source was not found in metadata');

      const capabilities =
        (await DataSource(httpClient).getDriverCapabilities(
          metadataSource.kind ?? ''
        )) ?? Feature.NotImplemented;

      /**
       * Handling click for GDC DBs
       */
      const isTableClicked = Object.keys(rest?.table || {}).length !== 0;
      const isFunctionClicked = Object.keys(rest?.function || {}).length !== 0;
      if (isTableClicked) {
        dispatch(
          _push(
            getRoute().table(
              database,
              rest.table,
              defaultTab(metadataSource.kind, capabilities)
            )
          )
        );
      } else if (isFunctionClicked) {
        dispatch(_push(getRoute().function(database, rest.function)));
      } else {
        dispatch(_push(getRoute().database(database)));
      }
    },
    [dispatch, httpClient, isUnmounted]
  );

  return { handleClick };
};
