import type { BigQueryTable } from '../../../DataSource';
import { getTableSchemaName } from './useTableSchema.utils';
import type { SqlTable } from './useTableSchema.utils';

describe('getTableSchemaName', () => {
  describe('when a SQLTable is provided', () => {
    it('returns the schema', () => {
      const table: SqlTable = { schema: 'aSchema', name: 'aName' };
      expect(getTableSchemaName(table)).toBe('aSchema');
    });
  });

  describe('when a BigQueryTable is provided', () => {
    it('returns the schema', () => {
      const table: BigQueryTable = { dataset: 'aDataset', name: 'aName' };
      expect(getTableSchemaName(table)).toBe('aDataset');
    });
  });
});
