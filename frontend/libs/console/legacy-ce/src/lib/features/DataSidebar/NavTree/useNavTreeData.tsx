import React from 'react';
import { useAvailableDrivers } from '../../ConnectDB';
import {
  useInconsistentMetadata,
  useMetadata,
} from '../../hasura-metadata-api';
import {
  adaptInconsistentObjects,
  adaptSourcesIntoTreeData,
} from './selectors';
import { multipleQueryUtils } from '../../Data/components/ReactQueryWrappers/utils';
import { DataSourceNode } from './types';

export const useNavTreeData = () => {
  // this is not a necessary part of the payload returned
  // this is just trying to provide a count of sources to accurately render a skeleton
  const { data: sourceCount } = useMetadata(m => m.metadata.sources.length);

  const availableDriversResult = useAvailableDrivers();

  const inconsistentResult = useInconsistentMetadata(adaptInconsistentObjects);

  const enableMetadataQuery =
    availableDriversResult.isSuccess && inconsistentResult.isSuccess;

  const metadataSelector = React.useCallback(
    m => {
      // returning here prevents re-running the expensive adapt function:
      if (!enableMetadataQuery) return [];

      return adaptSourcesIntoTreeData(m)(
        availableDriversResult.data,
        inconsistentResult.data
      );
    },
    [enableMetadataQuery, availableDriversResult.data, inconsistentResult.data]
  );

  const metadataResult = useMetadata(metadataSelector, {
    enabled: enableMetadataQuery,
  });

  const allResults = [
    availableDriversResult,
    inconsistentResult,
    metadataResult,
  ];

  const status = multipleQueryUtils.status(allResults);

  if (status === 'success') {
    return {
      status,
      error: multipleQueryUtils.firstError(allResults),
      treeData: metadataResult.data as DataSourceNode[],
      sourceCount,
    };
  }

  return {
    status,
    error: multipleQueryUtils.firstError(allResults),
    treeData: metadataResult.data,
    sourceCount,
  };
};
