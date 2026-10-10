import { EventRequestTransform } from '@hasura/shared/types';
import { LocalEventTriggerState } from '../../types';

export const eventTriggerState: LocalEventTriggerState = {
  name: 'trigger_name',
  table: {
    name: 'table_name',
    schema: 'public',
  },
  operations: ['INSERT'],
  operationColumns: ['name', 'id'],
  webhook: {
    type: 'static',
    value: 'http://httpbin.org/post',
  },
  retryConf: {
    num_retries: 10,
    interval_sec: 20,
    timeout_sec: 90,
  },
  headers: [
    {
      name: 'key',
      type: 'value',
      value: 'value',
    },
  ],
  source: 'default',
  isAllColumnChecked: true,
};

export const eventTriggerStateWithoutHeaders: LocalEventTriggerState = {
  name: 'trigger_name_1',
  table: {
    schema: 'public',
    name: 'table_name',
  },
  operations: ['INSERT'],
  operationColumns: [],
  retryConf: {
    interval_sec: 10,
    num_retries: 0,
    timeout_sec: 60,
  },
  source: 'default',
  isAllColumnChecked: false,
  headers: [],
  webhook: {
    value: 'http://httpbin.org/post',
    type: 'static',
  },
};

export const source = {
  name: 'default',
  driver: 'postgres',
} as const;

export const requestTransform: EventRequestTransform = {
  version: 2,
  template_engine: 'Kriti',
  method: 'GET',
  url: '{{$base_url}}/me',
  query_params: {
    userId: '123',
  },
  request_headers: {
    remove_headers: ['content-type'],
  },
  body: {
    action: 'transform',
    template:
      '{\n  "table": {\n    "name": {{$body.table.name}},\n    "schema": {{$body.table.schema}}\n  }\n}',
  },
};
