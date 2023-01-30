import React from 'react';
import { useQuery } from '@apollo/react-hooks';
import moment from 'moment';
import Modal from '../Common/Modal';

import { fetchOperationById, fetchSpansByRequestId } from './graphql.queries';

const LoadInspector = props => {
  const { projectId, requestId, time, transport, onHide, configData } = props;
  const variables = {
    variables: {
      projectId,
      requestId,
    },
  };

  const { loading, error, data } = useQuery(fetchOperationById, variables);

  const traceResponse = useQuery(fetchSpansByRequestId, {
    variables: {
      projectId,
      requestId,
      ...(time
        ? {
            fromTime: moment(time).subtract(5, 'minutes').toISOString(),
            toTime: moment(time).add(5, 'minutes').toISOString(),
          }
        : {}),
    },
  });

  if (loading) {
    return null;
  }

  if (error) {
    // Handle error
    return null;
  }

  if (!data || !data.operations || data.operations.length === 0) {
    return null;
  }

  if (traceResponse.loading) {
    return null;
  }

  if (traceResponse.error) {
    console.error(traceResponse.error);
    return null;
  }

  const trace = traceResponse?.data?.tracing_logs;

  const operation = {
    ...data.operations[0],
    transport,
    trace,
  };
  const nullData = operation.null_response_logs
    ? operation.null_response_logs[0]
    : null;

  return (
    <Modal
      data={operation}
      nullData={nullData}
      onHide={onHide}
      configData={configData}
    />
  );
};

export default LoadInspector;
