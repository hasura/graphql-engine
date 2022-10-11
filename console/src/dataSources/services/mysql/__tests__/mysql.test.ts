import { mysql } from '../index';

describe('mysql datasource tests', () => {
  describe('getAlterTableCommentSql', () => {
    const { getAlterTableCommentSql } = mysql;
    it('should generate SQL for modifying table comment', () => {
      const query = getAlterTableCommentSql({
        tableName: 'users',
        schemaName: 'public',
        comment: "user's comment",
      });
      expect(query).toContain('alter table');
      expect(query).toContain('`public`.`users`');
      expect(query).toContain("comment = '%string \"user's comment\" %'");
      expect(query).toMatchSnapshot();
    });
  });

  describe('getAlterColumnCommentSql', () => {
    const { getAlterColumnCommentSql } = mysql;
    it('should generate SQL for modifying column comment', () => {
      const query = getAlterColumnCommentSql({
        tableName: 'users',
        schemaName: 'public',
        columnName: 'id',
        columnType: 'Int',
        comment: "user's comment",
      });
      expect(query).toContain('alter table');
      expect(query).toContain('`public`.`users`');
      expect(query).toContain('modify column `id` Int');
      expect(query).toContain("comment '%string \"user's comment\" %'");
      expect(query).toMatchSnapshot();
    });
  });
});
