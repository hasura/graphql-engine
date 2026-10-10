import { QueryClient, useQueryClient } from '@tanstack/react-query';
import {
  INCONSISTENT_METADATA_QUERY_KEY,
  METADATA_QUERY_KEY,
} from '../constants';
import { useAppContext } from '@hasura/shared/context';

export type LogMetadataInvalidationProps = {
  componentName: string;
  reasons?: string[];
};

export const logMetadataInvalidation = ({
  componentName,
  reasons,
}: LogMetadataInvalidationProps) => {
  const logLabel = 'Invalidating Metadata...';
  console.groupCollapsed(logLabel);
  console.info(`Component: ${componentName}`);
  console.info(`Reasons:`);
  reasons?.forEach((reason) => console.info(`\t${reason}`));
  console.groupEnd();
};

export const useInvalidateMetadata = () => {
  const queryClient = useQueryClient();
  const { isProduction } = useAppContext();

  const invalidate = (props?: LogMetadataInvalidationProps) => {
    props?.componentName && !isProduction && logMetadataInvalidation(props);
    invalidateMetadata(queryClient);
  };

  return invalidate;
};

export const invalidateMetadata = (queryClient: QueryClient) => {
  queryClient.invalidateQueries({
    queryKey: [METADATA_QUERY_KEY],
  });
  queryClient.invalidateQueries({
    queryKey: [INCONSISTENT_METADATA_QUERY_KEY],
  });
};
