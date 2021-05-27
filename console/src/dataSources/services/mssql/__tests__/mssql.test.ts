import { mssql } from '../index';

describe('mssql datasource tests', () => {
  describe('supportedFeatures', () => {
    it('should have delete enabled', () => {
      expect(mssql?.supportedFeatures?.tables?.modify?.delete).toBe(true);
    });
  });
  describe('getDropSql', () => {
    const { getDropSql } = mssql;
    it('should generate query for table with default argument', () => {
      const query = getDropSql('users', 'public');
      expect(query).toContain(`DROP table "public"."users";`);
    });

    it('should support other properties ', () => {
      const query = getDropSql('users', 'public', 'view');
      expect(query).toContain(`DROP view "public"."users";`);

      const query2 = getDropSql('users', 'public', 'table');
      expect(query2).toContain(`DROP table "public"."users";`);
    });
  });
});
