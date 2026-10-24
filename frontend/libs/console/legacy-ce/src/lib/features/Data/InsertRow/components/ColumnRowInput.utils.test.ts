import { getFormatDateFn, hasTimeZone } from './ColumnRowInput.utils';

describe('hasTimeZone', () => {
  it.each`
    dataType                         | expected
    ${'timestamp with time zone'}    | ${true}
    ${'timestamptz'}                 | ${true}
    ${'time with time zone'}         | ${true}
    ${'timetz'}                      | ${true}
    ${'datetimeoffset'}              | ${true}
    ${'timestamp_tz'}                | ${true}
    ${'timestamp_ltz'}               | ${true}
    ${'datetime_with_timezone'}      | ${true}
    ${'time_with_timezone'}          | ${true}
    ${'timestamp_ntz'}               | ${false}
    ${'timestamp without time zone'} | ${false}
    ${'timestamp'}                   | ${false}
    ${'datetime'}                    | ${false}
    ${'datetime2'}                   | ${false}
    ${'time'}                        | ${false}
  `('$dataType keeps a time zone: $expected', ({ dataType, expected }) => {
    expect(hasTimeZone(dataType)).toBe(expected);
  });
});

describe('getFormatDateFn', () => {
  const date = new Date('2022-04-12T12:01:01');

  it.each`
    consoleDataType | withTimeZone | expected
    ${'timestamp'}  | ${false}     | ${'2022-04-12 12:01:01'}
    ${'timestamp'}  | ${true}      | ${'2022-04-12 12:01:01+00'}
    ${'time'}       | ${false}     | ${'12:01:01'}
    ${'time'}       | ${true}      | ${'12:01:01+00'}
  `(
    'formats a $consoleDataType (time zone: $withTimeZone) as $expected',
    ({ consoleDataType, withTimeZone, expected }) => {
      expect(getFormatDateFn(consoleDataType, withTimeZone)(date)).toBe(
        expected,
      );
    },
  );
});
