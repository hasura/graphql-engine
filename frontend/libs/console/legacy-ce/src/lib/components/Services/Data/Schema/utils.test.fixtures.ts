import { CheckDatabaseLatencyResponse } from '../../../../features/ConnectDB';

export const testDataOne: CheckDatabaseLatencyResponse = {
  taskEvent: {
    id: 'new-id',
    event_type: 'success',
    public_event_data: {
      sources: {
        default: {
          connection_source: 'PG_ENV_VAR',
          avg_latency: 445.23,
          error: '',
        },
        default2: {
          connection_source: 'PG_ENV_VAR_2',
          avg_latency: 3444.23,
          error: '',
        },
        default3: {
          connection_source: 'PG_ENV_VAR_3',
          avg_latency: 32,
          error: '',
        },
      },
    },
    error: 'string',
  },
  insertDbLatencyData: { data: { insert_db_latency_one: { id: 'test-id' } } },
};

export const testDataTwo: CheckDatabaseLatencyResponse = {
  taskEvent: {
    id: 'new-id',
    event_type: 'success',
    public_event_data: {
      sources: {
        default: {
          connection_source: 'PG_ENV_VAR',
          avg_latency: 44.23,
          error: '',
        },
        default2: {
          connection_source: 'PG_ENV_VAR_2',
          avg_latency: 3.23,
          error: '',
        },
        default3: {
          connection_source: 'PG_ENV_VAR_3',
          avg_latency: 31.98,
          error: '',
        },
      },
    },
    error: 'string',
  },
  insertDbLatencyData: { data: { insert_db_latency_one: { id: 'test-id' } } },
};
