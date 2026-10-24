import { getScalarConsoleDataType } from './utils';

describe('getScalarConsoleDataType', () => {
  // scalar types declared by the MySQL Super Connector agent
  it.each`
    scalarType                  | graphqlType  | expected
    ${'Int'}                    | ${'Int'}     | ${'integer'}
    ${'Float'}                  | ${'Float'}   | ${'float'}
    ${'Boolean'}                | ${'Boolean'} | ${'boolean'}
    ${'String'}                 | ${'String'}  | ${'string'}
    ${'date'}                   | ${'String'}  | ${'date'}
    ${'datetime'}               | ${'String'}  | ${'timestamp'}
    ${'datetime_with_timezone'} | ${'String'}  | ${'timestamp'}
    ${'time'}                   | ${'String'}  | ${'time'}
    ${'time_with_timezone'}     | ${'String'}  | ${'time'}
    ${'uuid'}                   | ${'ID'}      | ${'string'}
    ${'custom_number'}          | ${'Float'}   | ${'float'}
    ${'unknown'}                | ${undefined} | ${'string'}
  `(
    'maps $scalarType ($graphqlType) to $expected',
    ({ scalarType, graphqlType, expected }) => {
      expect(
        getScalarConsoleDataType(
          scalarType,
          graphqlType ? { graphql_type: graphqlType } : undefined,
        ),
      ).toBe(expected);
    },
  );
});
