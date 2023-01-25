import { useEffect } from 'react';

import { syncOperations } from './graphql.queries';
import { useMutation } from '@apollo/react-hooks';

const SyncWithAllowList = ({
  projectId,
  status,
  groupName,
  onCompleted,
  onError,
}) => {
  const [runSyncAllowListMutation] = useMutation(syncOperations, {
    variables: {
      projectId: projectId,
      name: groupName,
    },
    onCompleted: onCompleted,
    onError: onError,
  });
  useEffect(() => {
    if (status === 'Syncing') {
      runSyncAllowListMutation();
    }
  }, [status]);
  return null;
};

export default SyncWithAllowList;
