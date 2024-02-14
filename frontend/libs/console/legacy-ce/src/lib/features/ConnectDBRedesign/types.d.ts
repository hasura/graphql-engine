export type DatabaseConnection = {
  driver: string;
  details: {
    name: string;
    configuration: Record<string, any>; // This can vary from driver to driver
    customization: Record<string, any>;
  };
};

/**
 *
 * This type is a list of the types we care about for db connect.
 *
 * This is not a 1-1 correspondence with window.__env/globals
 *
 */
export type DbConnectConsoleType = 'oss' | 'pro' | 'pro-lite' | 'cloud';
export type DatabaseKind =
  | 'postgres'
  | 'mssql'
  | 'bigquery'
  | 'citus'
  | 'alloydb'
  | 'snowflake'
  | 'athena'
  | 'cockroach';

export type EEState = 'active' | 'inactive' | 'expired';

export type LatencyActionResponse = {
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
    } | null;
  };
  error: string;
};

export type Task = {
  id: string;
  name: string;
  task_events: TaskEvent[];
};

export type LatencyJobResponse = {
  data: {
    jobs_by_pk: {
      id: string;
      status: 'failed' | 'success' | 'running';
      tasks: Task[];
    };
  };
};

export type Latency = {
  dataSourceName: string;
  connectionSource: string;
  avgLatency: number;
  error: string;
};
