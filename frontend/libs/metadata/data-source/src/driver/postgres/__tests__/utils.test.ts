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
});
