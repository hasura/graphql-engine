import { adaptKeyConstraints } from './adaptKeys';

describe('adaptKeyConstraints', () => {
  it('parses run_sql rows into { constraintName, columns }[]', () => {
    const result = [
      ['table_name', 'table_schema', 'constraint_name', 'columns'],
      ['orders', 'public', 'orders_pkey', '{id}'],
      ['orders', 'public', 'orders_email_key', '{email,"tenant id"}'],
    ];
    expect(adaptKeyConstraints(result)).toEqual([
      { constraintName: 'orders_pkey', columns: ['id'] },
      { constraintName: 'orders_email_key', columns: ['email', 'tenant id'] },
    ]);
  });

  it('returns [] for empty / missing results', () => {
    expect(adaptKeyConstraints(undefined)).toEqual([]);
    expect(adaptKeyConstraints(null)).toEqual([]);
    expect(
      adaptKeyConstraints([
        ['table_name', 'table_schema', 'constraint_name', 'columns'],
      ]),
    ).toEqual([]);
  });
});
