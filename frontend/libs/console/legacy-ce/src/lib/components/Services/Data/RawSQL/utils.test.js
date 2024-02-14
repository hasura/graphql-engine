import { parseCreateSQL, removeCommentsSQL } from './utils';

describe('removeCommentsSQL', () => {
  it('removes comments', () => {
    const sql = `
-- Create tables in the public schema
CREATE TABLE public.locations (
  location_id SERIAL PRIMARY KEY, // comment
  name VARCHAR(100) NOT NULL,
  address VARCHAR(200) NOT NULL
);
/* another comment */
`;

    expect(removeCommentsSQL(sql).trim()).toEqual(
      `
CREATE TABLE public.locations (
  location_id SERIAL PRIMARY KEY, // comment
  name VARCHAR(100) NOT NULL,
  address VARCHAR(200) NOT NULL
);
`.trim()
    );
  });
});

describe('parseCreateSQL', () => {
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
    expect(parseCreateSQL(sql, 'postgres')).toEqual([
      {
        isPartition: false,
        name: 'locations',
        schema: 'public',
        type: 'table',
      },
    ]);
  });
});
