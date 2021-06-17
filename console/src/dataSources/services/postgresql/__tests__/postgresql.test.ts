import { postgres } from '../index';

describe('postgresql datasource tests', () => {
  describe('getAlterPkSql', () => {
    const { getAlterPkSql } = postgres;
    it('should generate alter operation as a single transaction ', () => {
      const query = getAlterPkSql({
        schemaName: 'public',
        tableName: 'users',
        selectedPkColumns: ['id'],
        constraintName: 'PK__users__1234',
      });
      expect(query).toContain('BEGIN TRANSACTION');
      expect(query).toContain('ALTER TABLE "public"."users"');
      expect(query).toContain('DROP CONSTRAINT "PK__users__1234"');
      expect(query).toContain('ADD CONSTRAINT "PK__users__1234"');
      expect(query).toContain('COMMIT TRANSACTION');
      expect(query).toMatchSnapshot();
    });
    it('should work with multi-column PKs ', () => {
      const query = getAlterPkSql({
        schemaName: 'public',
        tableName: 'users',
        selectedPkColumns: ['id', 'account'],
        constraintName: 'test_constraint',
      });
      expect(query).toContain(
        `ADD CONSTRAINT "test_constraint" PRIMARY KEY ("id", "account")`
      );
      expect(query).toMatchSnapshot();
    });
  });
});
