import { useCallback } from 'react';
import { useDispatch } from 'react-redux';
import { reloadMetadata } from '../../metadata/actions';

export const useReloadMetadata = () => {
  const dispatch = useDispatch();
  const reload = useCallback(
    ({
      shouldReloadAllSources = false,
      shouldReloadRemoteSchemas = false,
    }: {
      shouldReloadRemoteSchemas: boolean;
      shouldReloadAllSources: boolean;
    }) => {
      return new Promise<{ success: boolean }>((resolve, reject) => {
        try {
          dispatch(
            reloadMetadata(
              shouldReloadRemoteSchemas,
              shouldReloadAllSources,
              () => {
                resolve({ success: true });
              },
              () => {
                resolve({ success: false });
              }
            )
          );
        } catch (err) {
          reject(err);
        }
      });
    },
    [dispatch]
  );

  return { reloadMetadata: reload };
};
