import { mergeDataCitus } from '../mergeData';
import {
  citus_no_tables,
  citus_with_table,
  citus_with_relationships_fk,
} from './fixtures/input';

describe('verify merge data for citus', () => {
  test('citus with no tables', () => {
    const { query_data, metadata } = citus_no_tables;
    const res = mergeDataCitus(query_data, metadata);
    expect(res).toMatchSnapshot();
  });

  test('citus with table', () => {
    const { query_data, metadata } = citus_with_table;
    const res = mergeDataCitus(query_data, metadata);
    expect(res).toMatchSnapshot();
  });

  test('citus with relationships and foreign key', () => {
    const { query_data, metadata } = citus_with_relationships_fk;
    const res = mergeDataCitus(query_data, metadata);
    expect(res).toMatchSnapshot();
  });
});
