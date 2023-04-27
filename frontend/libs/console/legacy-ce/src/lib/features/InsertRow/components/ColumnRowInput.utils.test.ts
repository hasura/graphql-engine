import { getFormatDateFn } from './ColumnRowInput.utils';

describe('postgres format dates', () => {
  it.each`
    dataType                         | expected
    ${'datetime'}                    | ${'2022-04-12 12:01:01'}
    ${'timestamp without time zone'} | ${'2022-04-12 12:01:01'}
    ${'timestamp with time zone'}    | ${'2022-04-12 12:01:01+00'}
    ${'timestamp'}                   | ${'2022-04-12 12:01:01+00'}
    ${'time'}                        | ${'12:01:01'}
    ${'time without time zone'}      | ${'12:01:01'}
    ${'time with time zone'}         | ${'12:01:01+00'}
  `(
    'Given a "$dataType" path, then returns $expected',
    ({ dataType, expected }) => {
      expect(
        getFormatDateFn(dataType, 'postgres')(new Date('2022-04-12T12:01:01'))
      ).toBe(expected);
    }
  );
});

describe('mysql gdc format dates', () => {
  it.each`
    dataType                         | expected
    ${'datetime'}                    | ${'2022-04-12 12:01:01'}
    ${'timestamp without time zone'} | ${'2022-04-12 12:01:01'}
    ${'timestamp with time zone'}    | ${'2022-04-12 12:01:01+00'}
    ${'timestamp'}                   | ${'2022-04-12 12:01:01+00'}
    ${'time'}                        | ${'12:01:01'}
    ${'time without time zone'}      | ${'12:01:01'}
    ${'time with time zone'}         | ${'12:01:01+00'}
  `(
    'Given a "$dataType" path, then returns $expected',
    ({ dataType, expected }) => {
      expect(
        getFormatDateFn(dataType, 'mysql8')(new Date('2022-04-12T12:01:01'))
      ).toBe(expected);
    }
  );
});

describe('unknown driver format dates', () => {
  it.each`
    dataType                         | expected
    ${'datetime'}                    | ${'2022-04-12 12:01:01'}
    ${'timestamp without time zone'} | ${'2022-04-12 12:01:01'}
    ${'timestamp with time zone'}    | ${'2022-04-12 12:01:01+00'}
    ${'timestamp'}                   | ${'2022-04-12 12:01:01+00'}
    ${'time'}                        | ${'12:01:01'}
    ${'time without time zone'}      | ${'12:01:01'}
    ${'time with time zone'}         | ${'12:01:01+00'}
  `(
    'Given a "$dataType" path, then returns $expected',
    ({ dataType, expected }) => {
      expect(
        getFormatDateFn(
          dataType,
          'a-brand-new-driver'
        )(new Date('2022-04-12T12:01:01'))
      ).toBe(expected);
    }
  );
});
