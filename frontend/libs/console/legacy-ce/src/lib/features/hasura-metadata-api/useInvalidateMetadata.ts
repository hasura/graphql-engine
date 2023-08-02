import { useCallback } from 'react';
import { useQueryClient } from 'react-query';
import globals from '../../Globals';
import { METADATA_QUERY_KEY } from './useMetadata';

export type LogMetadataInvalidationProps = {
  componentName: string;
  reasons: string[];
};

export const logMetadataInvalidation = ({
  componentName,
  reasons,
}: LogMetadataInvalidationProps) => {
  if (!globals.isProduction) {
    const logLabel = 'Invalidating Metadata...';
    console.groupCollapsed(logLabel);
    console.info(`Component: ${componentName}`);
    console.info(`Reasons:`);
    reasons.forEach(reason => console.info(`\t${reason}`));
    console.groupEnd();
  }
};

export const useInvalidateMetadata = () => {
  const queryClient = useQueryClient();
  const invalidate = useCallback(
    (
      props: LogMetadataInvalidationProps & { additionalQueryKeys?: string[] }
    ) => {
      logMetadataInvalidation(props);
      queryClient.invalidateQueries([
        METADATA_QUERY_KEY,
        ...(props?.additionalQueryKeys ?? []),
      ]);
    },
    [queryClient]
  );
  return invalidate;
};
