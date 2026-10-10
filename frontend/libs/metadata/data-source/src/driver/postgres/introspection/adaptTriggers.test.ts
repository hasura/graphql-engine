import { adaptTriggers } from './adaptTriggers';

describe('adaptTriggers', () => {
  it('parses run_sql rows', () => {
    const result = [
      ['trigger_name', 'action_timing', 'events', 'action_statement'],
      [
        'set_updated_at',
        'BEFORE',
        'UPDATE',
        'EXECUTE FUNCTION set_updated_at()',
      ],
    ];
    expect(adaptTriggers(result)).toEqual([
      {
        name: 'set_updated_at',
        timing: 'BEFORE',
        events: 'UPDATE',
        definition: 'EXECUTE FUNCTION set_updated_at()',
      },
    ]);
  });

  it('includes the full CREATE TRIGGER statement when present', () => {
    const result = [
      [
        'trigger_name',
        'action_timing',
        'events',
        'action_statement',
        'trigger_definition',
      ],
      [
        'set_updated_at',
        'BEFORE',
        'UPDATE',
        'EXECUTE FUNCTION set_updated_at()',
        'CREATE TRIGGER set_updated_at BEFORE UPDATE ON orders FOR EACH ROW EXECUTE FUNCTION set_updated_at()',
      ],
    ];
    expect(adaptTriggers(result)[0].createStatement).toBe(
      'CREATE TRIGGER set_updated_at BEFORE UPDATE ON orders FOR EACH ROW EXECUTE FUNCTION set_updated_at()',
    );
  });

  it('handles empty / missing results', () => {
    expect(adaptTriggers(undefined)).toEqual([]);
    expect(adaptTriggers([['trigger_name']])).toEqual([]);
  });
});
