import { convertTableValue } from './InsertRowForm.utils';

describe('convertTableValue', () => {
  it.each`
    columnValue | dataType     | expected
    ${'foo'}    | ${'string'}  | ${'foo'}
    ${'13.2'}   | ${'number'}  | ${13.2}
    ${'11'}     | ${'number'}  | ${11}
    ${'true'}   | ${'boolean'} | ${true}
    ${'false'}  | ${'boolean'} | ${false}
    ${'true'}   | ${'bool'}    | ${true}
    ${'false'}  | ${'bool'}    | ${false}
  `(
    'given $dataType it returns $expected',
    ({ columnValue, dataType, expected }) => {
      expect(convertTableValue(columnValue, dataType)).toEqual(expected);
    }
  );
});
