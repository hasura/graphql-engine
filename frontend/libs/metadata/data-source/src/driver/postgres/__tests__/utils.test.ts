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
});
