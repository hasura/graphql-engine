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

  describe('getAlterPkSql', () => {
    const { getAlterPkSql } = mssql;
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

  describe('getAddColumnSql', () => {
    const { getAddColumnSql } = mssql;
    it('should generate SQL query for adding a column with nullable:false, unique:false with default', () => {
      const query = getAddColumnSql(
        'tableName',
        'schemaName',
        'columnName',
        'ColumnType',
        {
          nullable: false,
          unique: false,
          default: 'any',
        },
        'constraintName'
      );
      expect(query).toContain(
        `ALTER TABLE "schemaName"."tableName" ADD "columnName" ColumnType NOT NULL CONSTRAINT "constraintName" DEFAULT 'any' WITH VALUES`
      );
    });
    it('should generate SQL query for adding a column with nullable:true, unique:false with default', () => {
      const query = getAddColumnSql(
        'tableName',
        'schemaName',
        'columnName',
        'ColumnType',
        {
          nullable: true,
          unique: false,
          default: 'any',
        },
        'constraintName'
      );
      expect(query).toContain(
        'ALTER TABLE "schemaName"."tableName" ADD "columnName" ColumnType'
      );
      expect(query).toContain(
        `CONSTRAINT "constraintName" DEFAULT 'any' WITH VALUES`
      );
    });
    it('should generate SQL query for adding a column with nullable:true and unique:true with default', () => {
      const query = getAddColumnSql(
        'tableName',
        'schemaName',
        'columnName',
        'ColumnType',
        {
          nullable: true,
          unique: true,
          default: 'any',
        },
        'constraintName'
      );
      expect(query).toContain(
        'ALTER TABLE "schemaName"."tableName" ADD "columnName" ColumnType NULL UNIQUE'
      );
      expect(query).toContain(
        `CONSTRAINT "constraintName" DEFAULT 'any' WITH VALUES`
      );
    });
  });

  describe('getDropColumnSql', () => {
    const { getDropColumnSql } = mssql;
    it('should generate SQL query for droping a column', () => {
      const query = getDropColumnSql('tableName', 'schemaName', 'columnName');
      expect(query).toContain('ALTER TABLE "schemaName"."tableName"');
      expect(query).toContain('DROP COLUMN "columnName"');
      expect(query).toMatchSnapshot();
    });
  });

  describe('getAddUniqueConstraintSql', () => {
    const { getAddUniqueConstraintSql } = mssql;
    it('should generate SQL query for adding a unique constraint to multiple column', () => {
      const query = getAddUniqueConstraintSql(
        'tableName',
        'schemaName',
        'constraintName',
        ['column1', 'column2']
      );
      expect(query).toContain('ALTER TABLE "schemaName"."tableName"');
      expect(query).toContain('ADD CONSTRAINT "constraintName"');
      expect(query).toContain('UNIQUE (column1,column2)');
      expect(query).toMatchSnapshot();
    });
    it('should generate SQL query for adding a unique constraint to single column', () => {
      const query = getAddUniqueConstraintSql(
        'tableName',
        'schemaName',
        'constraintName',
        ['column1']
      );
      expect(query).toContain('ALTER TABLE "schemaName"."tableName"');
      expect(query).toContain('ADD CONSTRAINT "constraintName"');
      expect(query).toContain('UNIQUE (column1)');
      expect(query).toMatchSnapshot();
    });
  });

  describe('getCreatePkSql', () => {
    const { getCreatePkSql } = mssql;
    it('should generate SQL query for adding a pk constraint to multiple column', () => {
      const query = getCreatePkSql({
        schemaName: 'schemaName',
        tableName: 'tableName',
        selectedPkColumns: ['column1', 'column2'],
        constraintName: 'constraintName',
      });
      expect(query).toContain('ALTER TABLE "schemaName"."tableName"');
      expect(query).toContain('ADD CONSTRAINT "constraintName"');
      expect(query).toContain('PRIMARY KEY ("column1","column2")');
      expect(query).toMatchSnapshot();
    });
    it('should generate SQL query for adding a pk constraint to single column', () => {
      const query = getCreatePkSql({
        schemaName: 'schemaName',
        tableName: 'tableName',
        selectedPkColumns: ['column1'],
        constraintName: 'constraintName',
      });
      expect(query).toContain('ALTER TABLE "schemaName"."tableName"');
      expect(query).toContain('ADD CONSTRAINT "constraintName"');
      expect(query).toContain('PRIMARY KEY ("column1")');
      expect(query).toMatchSnapshot();
    });
  });

  describe('getRenameColumnQuery', () => {
    const { getRenameColumnQuery } = mssql;
    it('should generate SQL query for renaming a column', () => {
      const query = getRenameColumnQuery(
        'tableName',
        'schemaName',
        'newColumnName',
        'oldColumnName'
      );
      expect(query).toContain(
        `sp_rename '[schemaName].[tableName].[oldColumnName]', 'newColumnName', 'COLUMN'`
      );
      expect(query).toMatchSnapshot();
    });
  });

  describe('getAlterColumnTypeSql', () => {
    const { getAlterColumnTypeSql } = mssql;
    it('should generate SQL query for renaming a column with nullable as true', () => {
      const query = getAlterColumnTypeSql(
        'tableName',
        'schemaName',
        'columnName',
        'columnType',
        true
      );
      expect(query).toContain(
        'ALTER TABLE "schemaName"."tableName" ALTER COLUMN "columnName" columnType'
      );
    });
    it('should generate SQL query for renaming a column with nullable as false', () => {
      const query = getAlterColumnTypeSql(
        'tableName',
        'schemaName',
        'columnName',
        'columnType',
        false
      );
      expect(query).toContain(
        'ALTER TABLE "schemaName"."tableName" ALTER COLUMN "columnName" columnType'
      );
      expect(query).toContain('NOT NULL');
      expect(query).toMatchSnapshot();
    });
  });

  describe('getSetNotNullSql', () => {
    const { getSetNotNullSql } = mssql;
    it('should generate SQL query to set a column to not null', () => {
      const query = getSetNotNullSql(
        'tableName',
        'schemaName',
        'columnName',
        'columnType'
      );
      expect(query).toContain(
        'ALTER TABLE "schemaName"."tableName" ALTER COLUMN "columnName"  columnType NOT NULL'
      );
      expect(query).toMatchSnapshot();
    });
  });

  describe('getDropNotNullSql', () => {
    const { getDropNotNullSql } = mssql;
    it('should generate SQL query to set a column to drop not null', () => {
      const query = getDropNotNullSql(
        'tableName',
        'schemaName',
        'columnName',
        'columnType'
      );
      expect(query).toContain(
        'ALTER TABLE "schemaName"."tableName" ALTER COLUMN "columnName"  columnType NULL'
      );
      expect(query).toMatchSnapshot();
    });
  });

  describe('getSetColumnDefaultSql', () => {
    const { getSetColumnDefaultSql } = mssql;
    it('should generate SQL query to set or alter the default value to a column', () => {
      const query = getSetColumnDefaultSql(
        'tableName',
        'schemaName',
        'columnName',
        'defaultValue',
        'constraintName'
      );
      expect(query).toContain(
        'ALTER TABLE "schemaName"."tableName" DROP CONSTRAINT IF EXISTS "constraintName";'
      );
      expect(query).toContain(
        'ALTER TABLE "schemaName"."tableName" ADD CONSTRAINT "constraintName" DEFAULT defaultValue FOR "columnName"'
      );
      expect(query).toMatchSnapshot();
    });
  });

  describe('getDropColumnDefaultSql', () => {
    const { getDropColumnDefaultSql } = mssql;
    it('should generate SQL query to drop the default value to a column', () => {
      const query = getDropColumnDefaultSql(
        'tableName',
        'schemaName',
        'columnName',
        'constraintName'
      );
      expect(query).toContain(
        'ALTER TABLE "schemaName"."tableName" DROP CONSTRAINT "constraintName"'
      );
      expect(query).toMatchSnapshot();
    });
  });

  describe('getDropConstraintSql', () => {
    const { getDropConstraintSql } = mssql;
    it('should generate SQL query to drop the constraint to a column', () => {
      const query = getDropConstraintSql(
        'tableName',
        'schemaName',
        'constraintName'
      );
      expect(query).toContain(
        'ALTER TABLE "schemaName"."tableName" DROP CONSTRAINT "constraintName"'
      );
      expect(query).toMatchSnapshot();
    });
  });
});
