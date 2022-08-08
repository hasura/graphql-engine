import { exportMetadata } from '@/features/DataSource';
import { Dispatch } from '@/types';
import { GDC_TREE_VIEW_DEV } from '@/utils/featureFlags';
import { useCallback, useLayoutEffect, useRef } from 'react';

import _push from '../../push';

function useIsUnmounted() {
  const rIsUnmounted = useRef<'mounting' | 'mounted' | 'unmounted'>('mounting');

  useLayoutEffect(() => {
    rIsUnmounted.current = 'mounted';
    return () => {
      rIsUnmounted.current = 'unmounted';
    };
  }, []);

  return useCallback(() => rIsUnmounted.current !== 'mounted', []);
}

export const useGDCTreeClick = (dispatch: Dispatch) => {
  const isUnmounted = useIsUnmounted();

  const handleClick = useCallback(
    async (value: string[]) => {
      if (isUnmounted()) return;

      if (GDC_TREE_VIEW_DEV === 'disabled') return;

      const metadata = await exportMetadata();

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
    },
    [dispatch, isUnmounted]
  );

  return handleClick;
};
