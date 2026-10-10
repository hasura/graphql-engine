import {
  getCreatePrimaryKeySql,
  getAlterPrimaryKeySql,
  getCreateUniqueKeySql,
  getCreateCheckConstraintSql,
  getDropConstraintSql,
  getDropIndexSql,
  getCreateIndexSql,
} from './sqlQueries';

const table = { schema: 'public', name: 'orders' } as never;

describe('primary/unique key SQL (postgres family)', () => {
  it('creates a primary key with quoted identifiers', () => {
    expect(
      getCreatePrimaryKeySql({
        table,
        constraintName: 'orders_pkey',
        columns: ['id', 'tenant'],
      }),
    ).toBe(
      'alter table "public"."orders" add constraint "orders_pkey" primary key ("id", "tenant");',
    );
  });

  it('alters a primary key transactionally (drop + re-add)', () => {
    const sql = getAlterPrimaryKeySql({
      table,
      constraintName: 'orders_pkey',
      columns: ['id'],
    });
    expect(sql).toContain('BEGIN TRANSACTION;');
    expect(sql).toContain(
      'ALTER TABLE "public"."orders" DROP CONSTRAINT "orders_pkey";',
    );
    expect(sql).toContain('ADD CONSTRAINT "orders_pkey" PRIMARY KEY ("id");');
    expect(sql.trim().endsWith('COMMIT TRANSACTION;')).toBe(true);
  });

  it('creates a unique key with quoted identifiers', () => {
    expect(
      getCreateUniqueKeySql({
        table,
        constraintName: 'orders_email_key',
        columns: ['email'],
      }),
    ).toBe(
      'alter table "public"."orders" add constraint "orders_email_key" unique ("email");',
    );
  });
});

describe('index SQL (postgres family)', () => {
  it('creates a btree index', () => {
    expect(
      getCreateIndexSql({
        table,
        indexName: 'orders_email_idx',
        indexType: 'btree',
        columns: ['email'],
      }),
    ).toBe(
      'create index "orders_email_idx" on "public"."orders" using btree ("email");',
    );
  });

  it('creates a unique gin index on multiple columns', () => {
    expect(
      getCreateIndexSql({
        table,
        indexName: 'idx',
        indexType: 'gin',
        columns: ['a', 'b'],
        unique: true,
      }),
    ).toBe(
      'create unique index "idx" on "public"."orders" using gin ("a", "b");',
    );
  });

  it('drops an index if it exists', () => {
    expect(getDropIndexSql({ table, indexName: 'orders_email_idx' })).toBe(
      'drop index if exists "public"."orders_email_idx";',
    );
  });
});

describe('check constraint SQL (postgres family)', () => {
  it('builds an ALTER TABLE ADD CONSTRAINT ... CHECK statement', () => {
    expect(
      getCreateCheckConstraintSql({
        table,
        constraintName: 'positive_total',
        check: 'total > 0',
      }),
    ).toBe(
      'alter table "public"."orders" add constraint "positive_total" check (total > 0);',
    );
  });

  it('drops a constraint by name', () => {
    expect(
      getDropConstraintSql({ table, constraintName: 'positive_total' }).trim(),
    ).toBe('alter table "public"."orders" drop constraint "positive_total";');
  });
});
