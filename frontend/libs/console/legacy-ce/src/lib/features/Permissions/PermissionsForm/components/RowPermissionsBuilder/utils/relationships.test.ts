import { collectExistsTables, getNewExistsTablesToLoad } from './relationships';

// Regression coverage for issue #10907: `_exists` target tables must be queued
// for introspection so their `_where` columns/operators render in the builder.

describe('collectExistsTables', () => {
  it('finds a top-level _exists table', () => {
    expect(
      collectExistsTables({
        _exists: {
          _table: { schema: 'public', name: 'user_variables' },
          _where: { name: { _eq: 'tester' } },
        },
      }),
    ).toEqual([{ schema: 'public', name: 'user_variables' }]);
  });

  it('finds _exists tables nested under _and/_or/_not', () => {
    const tables = collectExistsTables({
      _and: [
        { isEnabled: { _eq: true } },
        {
          _or: [
            {
              _not: {
                _exists: {
                  _table: { schema: 'public', name: 'bans' },
                  _where: {},
                },
              },
            },
            {
              _exists: {
                _table: { schema: 'public', name: 'user_variables' },
                _where: {},
              },
            },
          ],
        },
      ],
    });

    expect(tables).toEqual([
      { schema: 'public', name: 'bans' },
      { schema: 'public', name: 'user_variables' },
    ]);
  });

  it('finds an _exists nested inside another _exists _where', () => {
    expect(
      collectExistsTables({
        _exists: {
          _table: { schema: 'public', name: 'outer' },
          _where: {
            _exists: {
              _table: { schema: 'public', name: 'inner' },
              _where: {},
            },
          },
        },
      }),
    ).toEqual([
      { schema: 'public', name: 'outer' },
      { schema: 'public', name: 'inner' },
    ]);
  });

  it('ignores an _exists with an empty _table (e.g. a fresh builder row)', () => {
    expect(
      collectExistsTables({ _exists: { _table: {}, _where: {} } }),
    ).toEqual([]);
  });

  it('returns an empty array for a filter without _exists', () => {
    expect(
      collectExistsTables({ name: { _eq: 'a' }, _and: [{ id: { _gt: 1 } }] }),
    ).toEqual([]);
  });
});

describe('getNewExistsTablesToLoad', () => {
  it('queues each _exists table under the given source', () => {
    expect(
      getNewExistsTablesToLoad({
        permissions: {
          _exists: {
            _table: { schema: 'public', name: 'user_variables' },
            _where: { name: { _eq: 'tester' } },
          },
        },
        tablesToLoad: [
          { table: { schema: 'public', name: 'games' }, source: 'default' },
        ],
        source: 'default',
      }),
    ).toEqual([
      {
        table: { schema: 'public', name: 'user_variables' },
        source: 'default',
      },
    ]);
  });

  it('does not re-queue a table already loaded for the same source', () => {
    expect(
      getNewExistsTablesToLoad({
        permissions: {
          _exists: {
            _table: { schema: 'public', name: 'user_variables' },
            _where: {},
          },
        },
        tablesToLoad: [
          {
            table: { schema: 'public', name: 'user_variables' },
            source: 'default',
          },
        ],
        source: 'default',
      }),
    ).toEqual([]);
  });

  it('de-duplicates repeated _exists references to the same table', () => {
    expect(
      getNewExistsTablesToLoad({
        permissions: {
          _and: [
            {
              _exists: {
                _table: { schema: 'public', name: 'bans' },
                _where: {},
              },
            },
            {
              _exists: {
                _table: { schema: 'public', name: 'bans' },
                _where: {},
              },
            },
          ],
        },
        tablesToLoad: [],
        source: 'default',
      }),
    ).toEqual([
      { table: { schema: 'public', name: 'bans' }, source: 'default' },
    ]);
  });
});
