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

  describe('getCreateIndexSQL', () => {
    const { createIndexSql } = postgres;
    it('generates proper SQL for column names containing no spaces', () => {
      if (createIndexSql) {
        const query = createIndexSql({
          table: { schema: 'test_schema', name: 'test_table' },
          columns: ['firstname', 'lastname'],
          indexName: 'test_index_name',
          indexType: 'btree',
          unique: false,
        });
        expect(query).toMatchInlineSnapshot(`
          "
            CREATE  INDEX \\"test_index_name\\" on
            \\"test_schema\\".\\"test_table\\" using btree (\\"firstname\\", \\"lastname\\");
          "
        `);
      }
    });

    it('generates proper SQL for column names in camel case', () => {
      if (createIndexSql) {
        const query = createIndexSql({
          table: { schema: 'test_schema', name: 'test_table' },
          columns: ['firstName', 'lastName'],
          indexName: 'test_index_name',
          indexType: 'btree',
          unique: false,
        });
        expect(query).toMatchInlineSnapshot(`
          "
            CREATE  INDEX \\"test_index_name\\" on
            \\"test_schema\\".\\"test_table\\" using btree (\\"firstName\\", \\"lastName\\");
          "
        `);
      }
    });

    it('generates proper SQL for column names containing spaces', () => {
      if (createIndexSql) {
        const query = createIndexSql({
          table: { schema: 'test_schema', name: 'test_table' },
          columns: ['first name', 'last name'],
          indexName: 'test_index_name',
          indexType: 'btree',
          unique: false,
        });
        expect(query).toMatchInlineSnapshot(`
          "
            CREATE  INDEX \\"test_index_name\\" on
            \\"test_schema\\".\\"test_table\\" using btree (\\"first name\\", \\"last name\\");
          "
        `);
      }
    });

    it('generates proper SQL for with unique param set to true', () => {
      if (createIndexSql) {
        const query = createIndexSql({
          table: { schema: 'test_schema', name: 'test_table' },
          columns: ['first name', 'last_name', 'fullName'],
          indexName: 'test_index_name',
          indexType: 'btree',
          unique: true,
        });
        expect(query).toMatchInlineSnapshot(`
          "
            CREATE UNIQUE INDEX \\"test_index_name\\" on
            \\"test_schema\\".\\"test_table\\" using btree (\\"first name\\", \\"last_name\\", \\"fullName\\");
          "
        `);
      }
    });
  });
});
