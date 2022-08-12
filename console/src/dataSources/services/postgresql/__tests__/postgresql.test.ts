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

  describe('getAlterTableCommentSql', () => {
    const { getAlterTableCommentSql } = postgres;
    it('should generate SQL for modifying table comment', () => {
      const query = getAlterTableCommentSql({
        tableName: 'users',
        schemaName: 'public',
        comment: "user's comment",
      });
      expect(query).toContain('comment on table');
      expect(query).toContain('"public"."users"');
      expect(query).toContain("E'user\\'s comment'");
      expect(query).toMatchSnapshot();
    });
  });

  describe('getAlterColumnCommentSql', () => {
    const { getAlterColumnCommentSql } = postgres;
    it('should generate SQL for modifying column comment', () => {
      const query = getAlterColumnCommentSql({
        tableName: 'users',
        schemaName: 'public',
        columnName: 'id',
        comment: "user's comment",
      });
      expect(query).toContain('comment on column');
      expect(query).toContain('"public"."users"."id"');
      expect(query).toContain("E'user\\'s comment'");
      expect(query).toMatchSnapshot();
    });
  });

  describe('getAlterFunctionCommentSql', () => {
    const { getAlterFunctionCommentSql } = postgres;
    it('should generate SQL for modifying function comment', () => {
      const query = getAlterFunctionCommentSql({
        functionName: 'users',
        schemaName: 'public',
        comment: "user's comment",
      });
      expect(query).toContain('comment on function');
      expect(query).toContain('"public"."users"');
      expect(query).toContain("E'user\\'s comment'");
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

  describe('getDataTriggerLogsCountQuery', () => {
    const { getDataTriggerLogsCountQuery } = postgres;
    if (getDataTriggerLogsCountQuery) {
      it('should generate SQL query for pending event count ', () => {
        const pendingCountQuery = getDataTriggerLogsCountQuery(
          'new_user',
          'pending'
        );
        expect(pendingCountQuery).toContain(
          'delivered=false AND error=false AND archived=false'
        );
        expect(pendingCountQuery).toContain(
          "data_table.trigger_name = 'new_user'"
        );
        expect(pendingCountQuery).toContain('FROM "hdb_catalog"."event_log"');
        expect(pendingCountQuery).toMatchSnapshot();
      });

      it('should generate SQL query for processed event count', () => {
        const processedCountQuery = getDataTriggerLogsCountQuery(
          'new_user',
          'processed'
        );
        expect(processedCountQuery).toContain(
          'AND (delivered=true OR error=true) AND archived=false'
        );
        expect(processedCountQuery).toContain(
          "data_table.trigger_name = 'new_user'"
        );
        expect(processedCountQuery).toContain('FROM "hdb_catalog"."event_log"');

        expect(processedCountQuery).toMatchSnapshot();
      });

      it('should generate SQL query for invocation event count', () => {
        const invocationCountQuery = getDataTriggerLogsCountQuery(
          'test_event',
          'invocation'
        );
        expect(invocationCountQuery).toContain(
          "data_table.trigger_name = 'test_event'"
        );
        expect(invocationCountQuery).toContain(
          'FROM "hdb_catalog"."event_invocation_logs"'
        );
        expect(invocationCountQuery).toMatchSnapshot();
      });
    }
  });

  describe('getDataTriggerLogsQuery', () => {
    const { getDataTriggerLogsQuery } = postgres;
    if (getDataTriggerLogsQuery) {
      it('should generate SQL query for pending event logs', () => {
        // pending
        const pendingLogsQuery = getDataTriggerLogsQuery(
          'pending',
          'new_user',
          10,
          10
        );
        expect(pendingLogsQuery).toContain(
          'delivered=false AND error=false AND archived=false'
        );
        expect(pendingLogsQuery).toContain(
          "data_table.trigger_name = 'new_user'"
        );
        expect(pendingLogsQuery).toContain('FROM "hdb_catalog"."event_log"');
        expect(pendingLogsQuery).toContain('LIMIT 10');
        expect(pendingLogsQuery).toContain('OFFSET 10');
        expect(pendingLogsQuery).toMatchSnapshot();
      });
      it('should generate SQL query for processed event logs', () => {
        // Processed
        const processedLogsQuery = getDataTriggerLogsQuery(
          'processed',
          'test_event',
          100,
          0
        );
        expect(processedLogsQuery).toContain(
          'AND (delivered=true OR error=true) AND archived=false'
        );
        expect(processedLogsQuery).toContain(
          "data_table.trigger_name = 'test_event'"
        );
        expect(processedLogsQuery).toContain('FROM "hdb_catalog"."event_log"');
        expect(processedLogsQuery).toContain('LIMIT 100');
        expect(processedLogsQuery).toContain('OFFSET 0');
        expect(processedLogsQuery).toMatchSnapshot();
      });
      it('should generate SQL query for event invocation logs', () => {
        // Invocation
        const invocationLogsQuery = getDataTriggerLogsQuery(
          'processed',
          'test_event',
          100,
          0
        );
        expect(invocationLogsQuery).toContain(
          'AND (delivered=true OR error=true) AND archived=false'
        );
        expect(invocationLogsQuery).toContain(
          "data_table.trigger_name = 'test_event'"
        );
        expect(invocationLogsQuery).toContain('FROM "hdb_catalog"."event_log"');
        expect(invocationLogsQuery).toContain('LIMIT 100');
        expect(invocationLogsQuery).toContain('OFFSET 0');
        expect(invocationLogsQuery).toMatchSnapshot();
      });
    }
  });
  describe('getDataTriggerInvocations', () => {
    const { getDataTriggerInvocations } = postgres;
    if (getDataTriggerInvocations) {
      it('should generate SQL to fetch invocations for an event', () => {
        // pending
        const pendingLogsQuery = getDataTriggerInvocations(
          '298f6a71-f503-46f1-814c-45daef0afe4d'
        );
        expect(pendingLogsQuery).toContain(
          "event_id = '298f6a71-f503-46f1-814c-45daef0afe4d'"
        );
        expect(pendingLogsQuery).toContain('created_at DESC NULLS LAST');
        expect(pendingLogsQuery).toContain(
          `FROM "hdb_catalog"."event_invocation_logs"`
        );
        expect(pendingLogsQuery).toMatchSnapshot();
      });
      it('should generate SQL to fetch invocations for an event with default source', () => {
        // pending
        const pendingLogsQuery = getDataTriggerInvocations(
          '298f6a71-f503-46f1-814c-45daef0afe4d'
        );
        expect(pendingLogsQuery).toContain(
          "event_id = '298f6a71-f503-46f1-814c-45daef0afe4d'"
        );
      });
    }
  });
});
