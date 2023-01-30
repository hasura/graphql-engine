import React from 'react';

import { useQuery } from '@apollo/react-hooks';

import Modal from './Modal';

import { fetchSubscriptionWorkerDetail } from './graphql.queries';

const LoadInspector = props => {
  const { projectId, pollerId, onHide } = props;
  const options = {
    variables: {
      projectId: projectId,
      pollerId: pollerId,
    },
    fetchPolicy: 'cache-and-network',
  };
  const { loading, error, data } = useQuery(
    fetchSubscriptionWorkerDetail,
    options
  );

  if (loading) {
    return null;
  }

  if (error) {
    // Handle error
    return null;
  }

  if (data && data.poller_logs_init.length === 0) {
    return null;
  }

  const poller_log = data.poller_logs_init[0];

  return <Modal data={poller_log} onHide={onHide} />;
};

export default LoadInspector;
