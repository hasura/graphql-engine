import { getReservedSessionVariables, validateUniqueParams } from '../utils';

describe('validateUniqueParams', () => {
  it('accepts IP, empty and custom session variables', () => {
    expect(validateUniqueParams('IP')).toBeNull();
    expect(validateUniqueParams(null)).toBeNull();
    expect(validateUniqueParams(['x-hasura-user-id'])).toBeNull();
  });

  it('rejects empty session variable rows', () => {
    expect(validateUniqueParams(['x-hasura-user-id', ' '])).toBe(
      'Session variables cannot be empty.',
    );
  });

  it('rejects system session variables', () => {
    expect(validateUniqueParams(['x-hasura-role'])).toBe(
      "System session variables can't be used as unique parameters: x-hasura-role",
    );
  });
});

describe('getReservedSessionVariables', () => {
  it('returns no reserved variables for IP or empty unique params', () => {
    expect(getReservedSessionVariables('IP')).toEqual([]);
    expect(getReservedSessionVariables(null)).toEqual([]);
    expect(getReservedSessionVariables(undefined)).toEqual([]);
    expect(getReservedSessionVariables([''])).toEqual([]);
  });

  it('allows custom session variables', () => {
    expect(
      getReservedSessionVariables(['x-hasura-user-id', 'x-hasura-org']),
    ).toEqual([]);
  });

  it('returns system session variables, case-insensitively', () => {
    expect(
      getReservedSessionVariables([
        'x-hasura-user-id',
        'X-Hasura-Role',
        'x-hasura-admin-secret',
      ]),
    ).toEqual(['X-Hasura-Role', 'x-hasura-admin-secret']);
  });
});
