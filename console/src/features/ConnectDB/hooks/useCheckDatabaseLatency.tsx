import { useQuery } from 'react-query';
import {
  controlPlaneClient,
  fetchDatabaseLatencyJobId,
  fetchInfoFromJobId,
  insertInfoIntoDBLatencyQuery,
} from '@/features/ControlPlane';
import globals from '@/Globals';
import { getProjectId } from '@/utils/cloudConsole';

type LatencyActionResponse = {
  data: {
    checkDBLatency: {
      db_latency_job_id: string;
    };
  };
};

export type TaskEvent = {
  id: string;
  event_type: 'success' | 'scheduled' | 'created' | 'running' | 'failure';
  public_event_data: {
    sources: {
      [sourceName: string]: {
        connection_source: string;
        avg_latency: number;
        error: string;
      };
    };
  };
  error: string;
};

type Task = {
  id: string;
  name: string;
  task_events: TaskEvent[];
};

type LatencyJobResponse = {
  data: {
    jobs_by_pk: {
      id: string;
      status: 'failed' | 'success' | 'running';
      tasks: Task[];
    };
  };
};

const client = controlPlaneClient();

const useCheckDatabaseLatencyRequest = (isEnabled: boolean) => {
  return useQuery({
    queryKey: ['latencyCheckJobSetup'],
    queryFn: async () => {
      const projectId = getProjectId(globals);

      if (!projectId) {
        return undefined;
      }

      return client.query<LatencyActionResponse>(fetchDatabaseLatencyJobId, {
        project_id: projectId,
      });
    },
    enabled: isEnabled,
  });
};

export const useCheckDatabaseLatency = (isEnabled: boolean) => {
  const { data: jobIdResponse, isSuccess } =
    useCheckDatabaseLatencyRequest(isEnabled);
  const projectId = getProjectId(globals);

  return useQuery({
    queryKey: [
      'latencyCheckJobSetup',
      jobIdResponse?.data?.checkDBLatency?.db_latency_job_id,
    ],
    queryFn: async () => {
      const dateStartRequest = new Date();

      const jobId = jobIdResponse?.data?.checkDBLatency?.db_latency_job_id;

      if (!jobId) {
        throw Error('Job ID was not found');
      }

      const jobStatusResponse = await client.query<LatencyJobResponse>(
        fetchInfoFromJobId,
        { id: jobId }
      );

      if (!jobStatusResponse?.data?.jobs_by_pk?.status) {
        throw Error(`status for job ${jobId} not available`);
      }

      if (jobStatusResponse.data.jobs_by_pk.status === 'failed') {
        const failedTaskEvent =
          jobStatusResponse?.data?.jobs_by_pk?.tasks?.[0]?.task_events?.find(
            taskEvent => taskEvent.event_type === 'failure'
          );
        return failedTaskEvent?.error;
      } else if (jobStatusResponse.data.jobs_by_pk.status === 'running') {
        throw Error(
          `the job(${jobId}) is still running, will refetch to get the latest latency data`
        );
      }

      const successTaskEvent =
        jobStatusResponse?.data?.jobs_by_pk?.tasks?.[0]?.task_events?.find(
          taskEvent => taskEvent.event_type === 'success'
        );

      if (!successTaskEvent) {
        throw Error('failed to complete latency check');
      }

      const dateDiff = new Date().getTime() - dateStartRequest.getTime();

      await client.query(insertInfoIntoDBLatencyQuery, {
        jobId,
        projectId,
        isLatencyDisplayed: true,
        datasDifferenceInMilliseconds: dateDiff,
      });

      return successTaskEvent;
    },
    enabled: isSuccess,
    retryDelay: 125,
    retry: 30,
  });
};
