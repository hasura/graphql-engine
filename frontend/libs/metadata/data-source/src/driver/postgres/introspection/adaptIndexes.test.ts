import { adaptIndexes } from './adaptIndexes';

describe('adaptIndexes', () => {
  it('parses run_sql rows', () => {
    const result = [
      ['index_name', 'index_type', 'index_columns', 'index_definition_sql'],
      ['orders_pkey', 'btree', '{id}', 'CREATE UNIQUE INDEX ...'],
      [
        'orders_tenant_email_idx',
        'btree',
        '{tenant_id,email}',
        'CREATE INDEX ...',
      ],
    ];
    expect(adaptIndexes(result)).toEqual([
      {
        name: 'orders_pkey',
        type: 'btree',
        columns: ['id'],
        definition: 'CREATE UNIQUE INDEX ...',
      },
      {
        name: 'orders_tenant_email_idx',
        type: 'btree',
        columns: ['tenant_id', 'email'],
        definition: 'CREATE INDEX ...',
      },
    ]);
  });

  it('handles empty / missing results', () => {
    expect(adaptIndexes(undefined)).toEqual([]);
    expect(adaptIndexes([['index_name']])).toEqual([]);
  });
});
