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
export type CheckDatabaseLatencyResponse = {
  taskEvent: TaskEvent;
  insertDbLatencyData:
    | {
        data: {
          insert_db_latency_one: {
            id: string;
          };
        };
      }
    | undefined;
};
