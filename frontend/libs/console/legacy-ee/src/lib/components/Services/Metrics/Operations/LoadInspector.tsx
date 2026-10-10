import { useQuery } from '@apollo/client/react';
import { add, sub } from 'date-fns';
import Modal from '../Common/Modal';

import { fetchOperationById, fetchSpansByRequestId } from './graphql.queries';

const LoadInspector = (props) => {
  const { projectId, requestId, time, transport, onHide } = props;
  const variables = {
    variables: {
      projectId,
      requestId,
    },
  };

  const {
    loading,
    error,
    data: rawData,
  } = useQuery(fetchOperationById, variables);

  const traceResponse = useQuery<any>(fetchSpansByRequestId, {
    variables: {
      projectId,
      requestId,
      ...(time
        ? {
            fromTime: sub(new Date(time), { minutes: 5 }).toISOString(),
            toTime: add(new Date(time), { minutes: 5 }).toISOString(),
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

  const data = rawData as any;
  if (!data || !data?.operations || data?.operations.length === 0) {
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

  return <Modal data={operation} onHide={onHide} />;
};

export default LoadInspector;
