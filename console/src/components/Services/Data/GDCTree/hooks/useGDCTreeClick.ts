import { Dispatch } from '@/types';
import { GDC_TREE_VIEW_DEV } from '@/utils/featureFlags';
import { useCallback } from 'react';
import _push from '../../push';

export const useGDCTreeClick = (dispatch: Dispatch) => {
  const handleClick = useCallback(
    value => {
      if (GDC_TREE_VIEW_DEV === 'disabled') return;

      const { database, ...table } = JSON.parse(value[0]);
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
    [dispatch]
  );

  return handleClick;
};
