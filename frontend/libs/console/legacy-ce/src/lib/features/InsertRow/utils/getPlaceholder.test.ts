import { dataSource } from '../../../dataSources';
import { getPlaceholder } from './getPlaceholder';

describe('getPlaceholder', () => {
  jest.useFakeTimers().setSystemTime(new Date('2020-01-01T13:45:00.000Z'));
  it.each`
    type                                    | expected
    ${dataSource.columnDataTypes.TIMESTAMP} | ${'2020-01-01T13:45:00.000Z'}
    ${dataSource.columnDataTypes.DATE}      | ${'2020-01-01'}
    ${dataSource.columnDataTypes.TIME}      | ${'13:45:00Z or 13:45:00+05:30'}
    ${dataSource.columnDataTypes.JSONDTYPE} | ${'{"name": "foo"} or [12, "bar"]'}
    ${dataSource.columnDataTypes.JSONB}     | ${'{"name": "foo"} or [12, "bar"]'}
    ${dataSource.columnDataTypes.ARRAY}     | ${'{"foo", "bar"} or ["foo", "bar"]'}
    ${dataSource.columnDataTypes.UUID}      | ${'xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx'}
  `('for type "$type" returns "$expected"', ({ type, expected }) => {
    expect(getPlaceholder(type)).toEqual(expected);
  });
});
