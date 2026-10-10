import { adaptCheckConstraints } from './adaptCheckConstraints';

describe('adaptCheckConstraints', () => {
  it('parses run_sql rows into one entry per constraint', () => {
    const result = [
      ['table_schema', 'table_name', 'constraint_name', 'check'],
      ['public', 'orders', 'nonneg_qty', 'CHECK (qty >= 0)'],
      ['public', 'orders', 'positive_total', 'CHECK (total > 0)'],
    ];

    expect(adaptCheckConstraints(result)).toEqual([
      { name: 'nonneg_qty', check: 'CHECK (qty >= 0)' },
      { name: 'positive_total', check: 'CHECK (total > 0)' },
    ]);
  });

  it('returns [] for empty / missing results', () => {
    expect(adaptCheckConstraints(undefined)).toEqual([]);
    expect(adaptCheckConstraints(null)).toEqual([]);
    expect(
      adaptCheckConstraints([
        ['table_schema', 'table_name', 'constraint_name', 'check'],
      ]),
    ).toEqual([]);
  });
});
