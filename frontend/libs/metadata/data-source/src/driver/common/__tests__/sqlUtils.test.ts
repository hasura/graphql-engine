import { removeCommentsSQL } from '../sqlUtils';

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
`.trim(),
    );
  });
});
