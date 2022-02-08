import {
  getRelColumnsMapping,
  getColumnNameArrayFromHookData,
  parseDbToDbRemoteRel,
} from '../utils';
import {
  column_mapping_input,
  column_name_data,
  parse_rel_data,
} from './fixtures/input';

describe('utils tests', () => {
  test('getRelColumnsMapping util function should return', () => {
    const res = getRelColumnsMapping(column_mapping_input);
    expect(res).toMatchSnapshot();
  });
  test('getColumnNameArrayFromHookData util function should return', () => {
    const res = getColumnNameArrayFromHookData(column_name_data);
    expect(res).toMatchSnapshot();
  });
  test('parseDbToDbRemoteRel util function should return', () => {
    const res = parseDbToDbRemoteRel(parse_rel_data);
    expect(res).toMatchSnapshot();
  });
});
