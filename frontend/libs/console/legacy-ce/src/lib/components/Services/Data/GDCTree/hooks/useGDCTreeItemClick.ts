import { exportMetadata } from '../../../../../features/DataSource';
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

      /**
       * Handling click for GDC DBs
       */
      const isTableClicked = Object.keys(rest?.table || {}).length !== 0;
      if (isTableClicked) {
        dispatch(
          _push(
            getRoute().table(
              database,
              rest.table,
              defaultTab(metadataSource.kind)
            )
          )
        );
      } else {
        dispatch(_push(getRoute().database(database)));
      }
    },
    [dispatch, httpClient, isUnmounted]
  );

  return { handleClick };
};
