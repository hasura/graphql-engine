import { mssql } from '../index';

describe('mssql datasource tests', () => {
  describe('supportedFeatures', () => {
    it('should have delete enabled', () => {
      expect(mssql?.supportedFeatures?.tables?.modify?.delete).toBe(true);
    });
    it('should have edit FK enabled', () => {
      expect(mssql?.supportedFeatures?.tables?.modify?.foreignKeys?.edit).toBe(
        true
      );
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

  describe('getAlterFKSql', () => {
    const { getAlterForeignKeySql } = mssql;
    it('should generate SQL query for altering foreign keys with one column', () => {
      const query = getAlterForeignKeySql(
        {
          tableName: 'user',
          schemaName: 'dbo',
          columns: ['id'],
        },
        {
          tableName: 'user1',
          schemaName: 'dbo',
          columns: ['id'],
        },
        'oldConstraint',
        'newConstraint',
        'no action',
        'cascade'
      );
      expect(query).toContain(`DROP CONSTRAINT IF EXISTS "oldConstraint"`);
      expect(query).toContain(`ADD CONSTRAINT "newConstraint"`);
      expect(query).toContain(` ON DELETE cascade`);
      expect(query).toContain(` ON UPDATE no action`);
      expect(query).toMatchSnapshot();
    });
    it('should generate SQL query for altering foreign keys with multiple columns', () => {
      const query = getAlterForeignKeySql(
        {
          tableName: 'user',
          schemaName: 'dbo',
          columns: ['id', 'id2'],
        },
        {
          tableName: 'user1',
          schemaName: 'dbo',
          columns: ['id', 'id2'],
        },
        'oldConstraint',
        'newConstraint',
        'cascade',
        'cascade'
      );
      expect(query).toContain(`DROP CONSTRAINT IF EXISTS "oldConstraint"`);
      expect(query).toContain(`ADD CONSTRAINT "newConstraint"`);
      expect(query).toContain('FOREIGN KEY (id, id2)');
      expect(query).toContain('REFERENCES "dbo"."user1" (id, id2)');
      expect(query).toContain('ON UPDATE cascade ON DELETE cascade');
      expect(query).toMatchSnapshot();
    });
  });

  describe('getCreateFKeySql', () => {
    const { getCreateFKeySql } = mssql;
    it('should generate query for create foreign keys with multiple columns', () => {
      const query = getCreateFKeySql(
        {
          tableName: 'user',
          schemaName: 'dbo',
          columns: ['id', 'id2'],
        },
        {
          tableName: 'user1',
          schemaName: 'dbo',
          columns: ['id', 'id2'],
        },
        'newConstraint',
        'cascade',
        'cascade'
      );
      expect(query).toContain('ALTER TABLE "dbo"."user"');
      expect(query).toContain('ADD CONSTRAINT "newConstraint"');
      expect(query).toContain('FOREIGN KEY (id, id2)');
      expect(query).toContain('REFERENCES "dbo"."user1" (id, id2)');
      expect(query).toContain('ON UPDATE cascade ON DELETE cascade');
      expect(query).toMatchSnapshot();
    });
    it('should generate query for create foreign keys with one columns', () => {
      const query = getCreateFKeySql(
        {
          tableName: 'user',
          schemaName: 'dbo',
          columns: ['id'],
        },
        {
          tableName: 'user1',
          schemaName: 'dbo',
          columns: ['id'],
        },
        'newConstraint',
        'cascade',
        'cascade'
      );
      expect(query).toContain('ALTER TABLE "dbo"."user"');
      expect(query).toContain('ADD CONSTRAINT "newConstraint"');
      expect(query).toMatchSnapshot();
    });
  });

  describe('getDropConstraintSql', () => {
    const { getDropConstraintSql } = mssql;
    it('should generate query for droping foreign keys', () => {
      const query = getDropConstraintSql(
        'tableName1',
        'schemaName',
        'constraintName'
      );
      expect(query).toContain(`DROP CONSTRAINT "constraintName"`);
      expect(query).toMatchSnapshot();
    });
  });
});
