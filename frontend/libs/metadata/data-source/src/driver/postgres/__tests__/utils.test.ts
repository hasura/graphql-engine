import { parsePostgresCreateSchemaSQL } from '../utils';

describe('parsePostgresCreateSchemaSQL', () => {
  it('return the correct tables', () => {
    const sql = `
-- Create tables in the public schema
CREATE TABLE public.locations (
  location_id SERIAL PRIMARY KEY, // comment
  name VARCHAR(100) NOT NULL,
  address VARCHAR(200) NOT NULL
);
/* another comment */
    `;
    expect(parsePostgresCreateSchemaSQL(sql)).toEqual([
      {
        isPartition: false,
        name: 'locations',
        schema: 'public',
        type: 'table',
      },
    ]);
  });

  it('reads identifiers containing $', () => {
    expect(
      parsePostgresCreateSchemaSQL('create table a10$_abcde (id int);'),
    ).toEqual([
      {
        isPartition: false,
        name: 'a10$_abcde',
        schema: 'public',
        type: 'table',
      },
    ]);
  });

  it('reads quoted identifiers with spaces', () => {
    expect(
      parsePostgresCreateSchemaSQL(
        'CREATE VIEW "My Schema"."Active Users" AS SELECT 1;',
      ),
    ).toEqual([
      {
        isPartition: false,
        name: 'Active Users',
        schema: 'My Schema',
        type: 'view',
      },
    ]);
  });

  it('reads `if not exists`, mixed quoting and several statements', () => {
    expect(
      parsePostgresCreateSchemaSQL(
        'create table if not exists s.t$1 (id int);\n' +
          'create view public."Mixed Case" as select 1;',
      ),
    ).toEqual([
      { isPartition: false, name: 't$1', schema: 's', type: 'table' },
      {
        isPartition: false,
        name: 'Mixed Case',
        schema: 'public',
        type: 'view',
      },
    ]);
  });

  it.each([
    ['a quote', `create table "x' or '1'='1" (id int);`],
    ['a semicolon', 'create table "a;drop table b" (id int);'],
    ['a hyphen', 'create table "my-table" (id int);'],
    ['a doubled quote', 'create table "a""b" (id int);'],
    ['a disallowed name after a schema', 'create table public."a;b" (id int);'],
    [
      'a disallowed name after if not exists',
      'create table if not exists "a;b" (id int);',
    ],
  ])('skips identifiers with %s', (_, sql) => {
    expect(parsePostgresCreateSchemaSQL(sql)).toEqual([]);
  });

  it('still reads the allowed statements next to a skipped one', () => {
    expect(
      parsePostgresCreateSchemaSQL(
        'create table "a;b" (id int); create table ok (id int);',
      ),
    ).toEqual([
      { isPartition: false, name: 'ok', schema: 'public', type: 'table' },
    ]);
  });
});
