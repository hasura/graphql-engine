import { convertTableValue } from './InsertRowForm.utils';

describe('convertTableValue', () => {
  it.each`
    columnValue   | consoleDataType | expected
    ${'foo'}      | ${'string'}     | ${'foo'}
    ${'13.2'}     | ${'number'}     | ${13.2}
    ${'11'}       | ${'integer'}    | ${11}
    ${'123.5'}    | ${'float'}      | ${123.5}
    ${'true'}     | ${'boolean'}    | ${true}
    ${'false'}    | ${'boolean'}    | ${false}
    ${'2022-1-1'} | ${'date'}       | ${'2022-1-1'}
    ${'42'}       | ${undefined}    | ${'42'}
  `(
    'given $consoleDataType it returns $expected',
    ({ columnValue, consoleDataType, expected }) => {
      expect(convertTableValue(columnValue, consoleDataType)).toEqual(expected);
    },
  );
});
