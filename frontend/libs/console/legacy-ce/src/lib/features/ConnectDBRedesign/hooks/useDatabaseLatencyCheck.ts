/**
 * This process works as follows
 * 1. The console submits it's "ProjectId" to lux.
 * 2. Lux gives back a "JobId" in return.
 * 3. At this point, the console starts a timer locally. Let's call this `start_time`0
 * 4. The console polls the "JobId" on lux until it's gets a response from lux.
 * 5. Stop the timer and calculate now() - start_time => "Latency"
 * 6. Save the "Latency" back to the server for keeping a record of it.
 * 7. Return the "Latency" info to the hook's consumer.
 */

import { useMutation, useQuery, UseQueryOptions } from 'react-query';
import globals from '../../../Globals';
import { getProjectId } from '../../../utils/cloudConsole';
import { CheckDatabaseLatencyResponse } from '../../ConnectDB/hooks';
import {
  controlPlaneClient,
  fetchDatabaseLatencyJobId,
  fetchInfoFromJobId,
  insertInfoIntoDBLatencyQuery,
} from '../../ControlPlane';
import { LatencyActionResponse, LatencyJobResponse } from '../types';

const getJobIdFromLux = async () => {
  const projectId = getProjectId(globals);

  if (!projectId) {
    return undefined;
  }

  return controlPlaneClient.query<LatencyActionResponse>(
    fetchDatabaseLatencyJobId,
    {
      project_id: projectId,
    }
  );
};

async function poll<ReturnType>(
  fn: () => Promise<ReturnType>,
  fnCondition: (result: ReturnType) => boolean,
  waitMs: number,
  maxPollNumber = 50
) {
  let iterCount = 0;
  let result = await fn();
  while (fnCondition(result) && iterCount < maxPollNumber) {
    await wait(waitMs);
    result = await fn();
    iterCount++;
  }
  return result;
}

function wait(ms = 1000) {
  return new Promise(resolve => {
    setTimeout(resolve, ms);
  });
}

const getLatencyPingInfo = async (jobId: string) => {
  const fn = () =>
    controlPlaneClient.query<LatencyJobResponse>(fetchInfoFromJobId, {
      id: jobId,
    });

  const fnCondition = (jobStatusResponse: LatencyJobResponse) =>
    jobStatusResponse.data.jobs_by_pk.status === 'running';

  const finalResult = await poll(fn, fnCondition, 1000);

  return finalResult;
};

type DbLatencyMutationProps = {
  dateDifferenceInMilliseconds: number;
  projectId: string | undefined;
  jobId: string;
};

type LatencyData = CheckDatabaseLatencyResponse['insertDbLatencyData'];
const useInsertIntoDBLatencyTable = () => {
  return useMutation({
    mutationFn: async (props: DbLatencyMutationProps): Promise<LatencyData> =>
      controlPlaneClient.query<LatencyData>(insertInfoIntoDBLatencyQuery, {
        isLatencyDisplayed: true,
        ...props,
      }),
    retry: 1,
  });
};

type QueryOptions = Omit<UseQueryOptions, 'queryFn'>;

export const useDatabaseLatencyCheck = (props: QueryOptions) => {
  const insertDbLatencyMutation = useInsertIntoDBLatencyTable();

  return useQuery({
    queryKey: ['database_latency_check'],
    queryFn: async () => {
      // only for testing
      // return {
      //   latencies: [
      //     {
      //       dataSourceName: 'sqlite_test',
      //       avgLatency: 150,
      //       connectionSource: 'env_var',
      //       error: '',
      //     },
      //     {
      //       dataSourceName: 'chinook',
      //       avgLatency: 90,
      //       connectionSource: 'env_var',
      //       error: '',
      //     },
      //     {
      //       dataSourceName: 'mssql1',
      //       avgLatency: 270,
      //       connectionSource: 'env_var',
      //       error: '',
      //     },
      //   ],
      //   rowId: 'somerowId',
      // };

      // Get Job Id from lux
      const resultFromLux = await getJobIdFromLux();

      // Start timer
      const startTime = new Date().getTime();

      // const JobId
      const jobId = resultFromLux?.data?.checkDBLatency?.db_latency_job_id;

      if (!jobId) {
        throw Error('Job ID was not found');
      }

      // poll the job id
      const latencyResponse = await getLatencyPingInfo(jobId);

      if (!latencyResponse?.data?.jobs_by_pk?.status) {
        throw Error(`status for job ${jobId} not available`);
      }

      if (latencyResponse.data.jobs_by_pk.status === 'failed') {
        const failedTaskEvent =
          latencyResponse?.data?.jobs_by_pk?.tasks?.[0]?.task_events?.find(
            taskEvent => taskEvent.event_type === 'failure'
          );
        throw Error(failedTaskEvent?.error);
      }

      const taskEvent =
        latencyResponse?.data?.jobs_by_pk?.tasks?.[0]?.task_events?.find(
          taskEvent => taskEvent.event_type === 'success'
        );

      /**
       * handle internal error of successful taskq job where
       * taskq failed to connect to the project's data source
       */
      if (taskEvent?.public_event_data?.sources?.default?.error) {
        throw new Error(taskEvent?.public_event_data?.sources?.default?.error);
      }

      // Save this data back to lux
      insertDbLatencyMutation.mutate({
        dateDifferenceInMilliseconds: new Date().getTime() - startTime,
        projectId: getProjectId(globals),
        jobId,
      });

      const latencies = Object.entries(
        taskEvent?.public_event_data.sources ?? {}
      ).map(([source, latencyInfo]) => {
        return {
          dataSourceName: source,
          connectionSource: latencyInfo.connection_source,
          avgLatency: latencyInfo.avg_latency,
          error: latencyInfo.error,
        };
      });

      return {
        latencies,
        rowId: insertDbLatencyMutation.data?.data.insert_db_latency_one.id,
      };
    },
    enabled: props.enabled,
    onSuccess: data => {
      props.onSuccess?.(data);
    },
    onError: props.onError,
  });
};
