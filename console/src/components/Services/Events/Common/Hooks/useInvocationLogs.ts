import React from 'react';
import requestAction from '../../../../../utils/requestAction';
import Endpoints from '../../../../../Endpoints';
import { SelectQueryGenerator } from '../../../../Common/utils/v1QueryUtils';
import { useInterval } from '../../../../Common/utils/reactUtils';

export const invocationLogsColumns = [
  'id',
  'event_id',
  'status',
  'request',
  'response',
  'created_at',
];

type InvocationLog = {
  id: string;
  event_id: string;
  status: number;
  request: any;
  response: any;
  created_at: string;
};

type LoadAction = 'older' | 'newer';

export const useInvocationLogs = (
  triggerName: string,
  queryGenerator: SelectQueryGenerator,
  dispatch: any
) => {
  const [logs, setLogs] = React.useState([] as InvocationLog[]);
  const [limit] = React.useState(10);
  const [loading, setLoading] = React.useState(false);
  const [error, setError] = React.useState(null as any | null);
  const [oldLogsExist, setOldLogsExist] = React.useState(false);
  const [newLogsExist, setNewLogsExist] = React.useState(false);

  const getNewestId = () => {
    if (logs.length) {
      return logs[0].id;
    }
    return '';
  };

  const getOldestTimestamp = () => {
    if (logs.length) {
      return logs[logs.length - 1].created_at;
    }
    return 'now()';
  };

  const getNewestTimestamp = () => {
    if (logs.length) {
      return logs[0].created_at;
    }
    return 'now()';
  };

  const getAllIds = () => {
    return logs
      .filter(l => l.created_at === getOldestTimestamp())
      .map(l => l.id);
  };

  const getQuery = (action: LoadAction) => {
    const whereClauses: any[] = [
      {
        id: {
          $nin: getAllIds(),
        },
      },
      {
        scheduled_event: {
          scheduled_trigger: {
            name: triggerName,
          },
        },
      },
    ];
    if (action === 'older') {
      whereClauses.push({
        created_at: {
          $lte: getOldestTimestamp(),
        },
      });
    }
    if (action === 'newer') {
      whereClauses.push({
        created_at: {
          $gt: getNewestTimestamp(),
        },
      });
    }
    return queryGenerator(
      {
        $and: whereClauses,
      },
      undefined,
      [
        {
          column: 'created_at',
          type: 'desc',
        },
      ],
      limit
    );
  };

  const fetchLogs = (action: LoadAction) => {
    setLoading(true);
    dispatch(
      requestAction(
        Endpoints.query,
        {
          method: 'POST',
          body: JSON.stringify(getQuery(action)),
        },
        undefined,
        undefined,
        undefined,
        true
      )
    ).then(
      (data: InvocationLog[]) => {
        if (action === 'newer') {
          setNewLogsExist(false);
          setLogs(l => [...data, ...l]);
        }
        if (action === 'older') {
          if (data.length < 10) {
            setOldLogsExist(false);
          } else {
            setOldLogsExist(true);
          }
          setLogs(l => [...l, ...data]);
        }
        setLoading(false);
      },
      (e: any) => {
        setError(e);
        setLoading(false);
      }
    );
  };

  const fetchNewestLog = () => {
    const query = queryGenerator(
      {
        scheduled_event: {
          scheduled_trigger: {
            name: triggerName,
          },
        },
      },
      undefined,
      [{ column: 'created_at', type: 'desc' }],
      1
    );

    dispatch(
      requestAction(
        Endpoints.query,
        {
          method: 'POST',
          body: JSON.stringify(query),
        },
        undefined,
        undefined,
        undefined,
        true
      )
    ).then(
      (data: InvocationLog[]) => {
        if (data.length) {
          if (data[0].id !== getNewestId()) {
            setNewLogsExist(true);
          }
        }
      },
      (e: any) => {
        setError(e);
        setLoading(false);
      }
    );
  };

  React.useEffect(() => {
    fetchLogs('older');
  }, [triggerName]);

  useInterval(fetchNewestLog, 5000);

  return {
    logs,
    error,
    loading,
    fetchLogs,
    oldLogsExist,
    newLogsExist,
  };
};
