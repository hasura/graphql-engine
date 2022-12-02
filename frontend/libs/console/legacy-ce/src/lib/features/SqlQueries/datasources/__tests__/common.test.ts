import { QualifiedTable } from '../../../../metadata/types';
import { getSchemasWhereClause, getTablesWhereClause } from '../common';

describe('Generate Where Clauses', () => {
  it('generates where clause for schemas', () => {
    const schemas = ['public', 'books'];
    const where = getSchemasWhereClause(schemas);
    expect(where('nspm.relname')).toMatchSnapshot();
    expect(where('t.schema_name')).toMatchSnapshot();
    const andClause = getSchemasWhereClause(schemas, 'AND');
    expect(andClause('nspm.relname')).toMatchSnapshot();
    expect(andClause('t.schema_name')).toMatchSnapshot();
  });

  it('generates where clause for tables', () => {
    const tables: QualifiedTable[] = [
      { name: 'users', schema: 'public' },
      { name: 'debtors', schema: 'finance' },
      { name: 'employees', schema: 'human_resources' },
    ];
    const where = getTablesWhereClause(tables);
    expect(
      where({ schema: 'nspm.relname', name: 'pgc.relname' })
    ).toMatchSnapshot();
    expect(
      where({ schema: 't.schema_name', name: 't.table_name' })
    ).toMatchSnapshot();
    const andClause = getTablesWhereClause(tables, 'AND');
    expect(
      andClause({ schema: 'nspm.relname', name: 'pgc.relname' })
    ).toMatchSnapshot();
    expect(
      andClause({ schema: 't.schema_name', name: 't.table_name' })
    ).toMatchSnapshot();
  });

  it('generates where clause for multiple sql schemas', () => {
    const schemas = ['public', 'books'];
    const where = getSchemasWhereClause(schemas);
    expect(where(['nspm.relname', 't.schema_name'])).toMatchSnapshot();
    const andClause = getSchemasWhereClause(schemas, 'AND');
    expect(andClause(['nspm.relname', 't.schema_name'])).toMatchSnapshot();
  });

  it('generates where clause for multiple sql tables', () => {
    const tables: QualifiedTable[] = [
      { name: 'users', schema: 'public' },
      { name: 'debtors', schema: 'finance' },
      { name: 'employees', schema: 'human_resources' },
    ];
    const where = getTablesWhereClause(tables);
    expect(
      where([
        { schema: 'nspm.relname', name: 'pgc.relname' },
        { schema: 't.schema_name', name: 't.table_name' },
      ])
    ).toMatchSnapshot();
    const andClause = getTablesWhereClause(tables, 'AND');
    expect(
      andClause([
        { schema: 'nspm.relname', name: 'pgc.relname' },
        { schema: 't.schema_name', name: 't.table_name' },
      ])
    ).toMatchSnapshot();
  });

  it('returns empty string when any array of the params is empty', () => {
    const schemas = ['public', 'books'];
    const where = getSchemasWhereClause(schemas);
    expect(where([])).toEqual('');
    const andClause = getSchemasWhereClause([], 'AND');
    expect(andClause(['nspm.relname', 't.schema_name'])).toEqual('');

    const tables: QualifiedTable[] = [
      { name: 'users', schema: 'public' },
      { name: 'debtors', schema: 'finance' },
      { name: 'employees', schema: 'human_resources' },
    ];
    const where1 = getTablesWhereClause(tables);
    expect(where1([])).toEqual('');
    const andClause1 = getTablesWhereClause([], 'AND');
    expect(
      andClause1([
        { schema: 'nspm.relname', name: 'pgc.relname' },
        { schema: 't.schema_name', name: 't.table_name' },
      ])
    ).toEqual('');
  });
});
