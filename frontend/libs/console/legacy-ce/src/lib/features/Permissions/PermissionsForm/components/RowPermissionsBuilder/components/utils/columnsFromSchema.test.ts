import { columnsFromSchema } from './columnsFromSchema';
import { schema, createType } from '../__tests__/fixtures/graphql';

describe('columnsFromSchema', () => {
  it('should return columns from schema', () => {
    const result = columnsFromSchema(schema);
    const columns = result['Artist'];
    expect(columns).toEqual([
      {
        name: 'ArtistId',
        type: 'number_SQLite_comparison_exp',
        graphQLType: createType('number_SQLite_comparison_exp'),
      },
      {
        name: 'Name',
        type: 'string_SQLite_comparison_exp',
        graphQLType: createType('string_SQLite_comparison_exp'),
      },
    ]);
  });
});
