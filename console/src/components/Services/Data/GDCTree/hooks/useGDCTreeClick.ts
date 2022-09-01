import { exportMetadata } from '@/features/DataSource';
import { useHttpClient } from '@/features/Network';
import { useFireNotification } from '@/new-components/Notifications';
import { Dispatch } from '@/types';
import { GDC_TREE_VIEW_DEV } from '@/utils/featureFlags';
import { useCallback } from 'react';

import _push from '../../push';
import { useIsUnmounted } from '../utils';

export const useGDCTreeClick = (dispatch: Dispatch) => {
  const isUnmounted = useIsUnmounted();
  const httpClient = useHttpClient();
  const { fireNotification } = useFireNotification();

  const handleClick = useCallback(
    async (value: string[]) => {
      try {
        if (isUnmounted()) return;

        if (GDC_TREE_VIEW_DEV === 'disabled') return;

        const { metadata } = await exportMetadata({ httpClient });

        const { database, ...table } = JSON.parse(value[0]);

        const metadataSource = metadata.sources.find(
          source => source.name === database
        );

        if (!metadataSource)
          throw Error('useGDCTreeClick: source was not found in metadata');

        /**
         * Handling click for GDC DBs
         */
        const isTableClicked = Object.keys(table).length !== 0;
        if (isTableClicked) {
          dispatch(
            _push(
              encodeURI(
                `/data/v2/manage?database=${database}&table=${JSON.stringify(
                  table
                )}`
              )
            )
          );
        } else {
          dispatch(_push(encodeURI(`/data/v2/manage?database=${database}`)));
        }
      } catch (err) {
        fireNotification({
          type: 'error',
          title: 'Could handle database selection',
          message: JSON.stringify(err),
        });
      }
    },
    [dispatch, httpClient, isUnmounted]
  );

  return handleClick;
};
