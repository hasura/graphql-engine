import { exportMetadata } from '@/features/DataSource';
import { useHttpClient } from '@/features/Network';
import { Dispatch } from '@/types';
import { useCallback } from 'react';
import _push from './push';

export const useGDCTreeItemClick = (dispatch: Dispatch) => {
  const httpClient = useHttpClient();

  const handleClick = useCallback(
    async value => {
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
    },
    [dispatch, httpClient]
  );

  return { handleClick };
};
