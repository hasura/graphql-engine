import { useEffect } from 'react';
import { getCurrentReduxResourceVersion } from '../../store/utils';
import { useInvalidateMetadata } from './useInvalidateMetadata';
import { useMetadata } from './useMetadata';

/**
 *
 * This hook checks if the react-query resource_version is out of sync with the redux store on component mount
 * If it is, it performs a react-query invalidation of the metadata key and logs the action.
 *
 */
export const useSyncResourceVersionOnMount = ({
  componentName,
}: {
  componentName: string;
}) => {
  const { data: reactQueryResourceVersion } = useMetadata(
    m => m.resource_version
  );

  const reduxResourceVersion = getCurrentReduxResourceVersion();
  const invalidateMetadata = useInvalidateMetadata();

  useEffect(() => {
    if (
      reduxResourceVersion &&
      reactQueryResourceVersion &&
      reduxResourceVersion > reactQueryResourceVersion
    ) {
      invalidateMetadata({
        componentName,
        reasons: [
          `(useSyncResourceVersionOnMount)`,
          `Resource versions detected to be out of sync on component mount...`,
          `redux: ${reduxResourceVersion}`,
          `react-query: ${reactQueryResourceVersion}`,
        ],
      });
    }
  }, []);
};
