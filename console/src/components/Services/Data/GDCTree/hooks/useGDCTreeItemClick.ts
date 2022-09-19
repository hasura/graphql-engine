import { exportMetadata } from '@/features/DataSource';
import { useHttpClient } from '@/features/Network';
import { Dispatch } from '@/types';
import { useCallback } from 'react';
import { useIsUnmounted } from '../../Common/tsUtils';
import _push from '../../push';

export const useGDCTreeItemClick = (dispatch: Dispatch) => {
  const httpClient = useHttpClient();
  const isUnmounted = useIsUnmounted();

  const handleClick = useCallback(
    async value => {
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
            encodeURI(
              `/data/v2/manage?database=${database}&table=${JSON.stringify(
                rest.table
              )}`
            )
          )
        );
      } else {
        dispatch(_push(encodeURI(`/data/v2/manage?database=${database}`)));
      }
    },
    [dispatch, httpClient, isUnmounted]
  );

  return { handleClick };
};
