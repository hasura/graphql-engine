import { generateCreateEventTriggerQuery } from '../queryUtils';
import {
  eventTriggerState,
  eventTriggerStateWithoutHeaders,
  requestTransform,
  source,
} from './fixtures/input';

describe('Testing generateCreateEventTriggerQuery while creating ET', () => {
  it('with request transform', () => {
    const res = generateCreateEventTriggerQuery(
      eventTriggerState,
      source,
      false,
      requestTransform
    );
    expect(res?.args?.headers).toEqual([{ name: 'key', value: 'value' }]);
    expect(res?.args?.request_transform).toEqual({
      version: 2,
      template_engine: 'Kriti',
      method: 'GET',
      url: '{{$base_url}}/me',
      query_params: { userId: '123' },
      request_headers: { remove_headers: ['content-type'] },
      body: {
        action: 'transform',
        template:
          '{\n' +
          '  "table": {\n' +
          '    "name": {{$body.table.name}},\n' +
          '    "schema": {{$body.table.schema}}\n' +
          '  }\n' +
          '}',
      },
    });
    expect(res).toMatchSnapshot();
  });
  it('without request transform', () => {
    const res = generateCreateEventTriggerQuery(
      eventTriggerState,
      source,
      false
    );
    expect(res?.args?.request_transform).toEqual(undefined);
    expect(res).toMatchSnapshot();
  });
  it('without request transform and headers', () => {
    const res = generateCreateEventTriggerQuery(
      eventTriggerStateWithoutHeaders,
      source,
      false
    );
    expect(res?.args?.headers).toEqual([]);
    expect(res?.args?.request_transform).toEqual(undefined);
    expect(res).toMatchSnapshot();
  });
});

describe('Testing generateCreateEventTriggerQuery while mofifying ET', () => {
  it('with request transform', () => {
    const res = generateCreateEventTriggerQuery(
      eventTriggerState,
      source,
      true,
      requestTransform
    );
    expect(res?.args?.headers).toEqual([{ name: 'key', value: 'value' }]);
    expect(res?.args?.request_transform).toEqual({
      version: 2,
      template_engine: 'Kriti',
      method: 'GET',
      url: '{{$base_url}}/me',
      query_params: { userId: '123' },
      request_headers: { remove_headers: ['content-type'] },
      body: {
        action: 'transform',
        template:
          '{\n' +
          '  "table": {\n' +
          '    "name": {{$body.table.name}},\n' +
          '    "schema": {{$body.table.schema}}\n' +
          '  }\n' +
          '}',
      },
    });
    expect(res).toMatchSnapshot();
  });
  it('without request transform', () => {
    const res = generateCreateEventTriggerQuery(
      eventTriggerState,
      source,
      true
    );
    expect(res?.args?.request_transform).toEqual(undefined);
    expect(res).toMatchSnapshot();
  });
  it('without request transform and headers', () => {
    const res = generateCreateEventTriggerQuery(
      eventTriggerStateWithoutHeaders,
      source,
      true
    );
    expect(res?.args?.headers).toEqual([]);
    expect(res?.args?.request_transform).toEqual(undefined);
    expect(res).toMatchSnapshot();
  });
});
