import {
  ClientHeader,
  clientHeaderSchema,
  HeaderConfig,
} from '@hasura/shared/types';
import { parseHeaderConfigs, transformHeaderConfigs } from './schema';

describe('clientHeaderSchema', () => {
  it('accepts a well-formed value header', () => {
    const result = clientHeaderSchema.safeParse({
      name: 'x-hasura-role',
      value: 'admin',
      type: 'value',
    });
    expect(result.success).toBe(true);
  });

  it('accepts a well-formed env header', () => {
    const result = clientHeaderSchema.safeParse({
      name: 'x-hasura-role',
      value: 'MY_ENV',
      type: 'env',
    });
    expect(result.success).toBe(true);
  });

  it('rejects an unknown type', () => {
    const result = clientHeaderSchema.safeParse({
      name: 'x-hasura-role',
      value: 'admin',
      type: 'literal',
    });
    expect(result.success).toBe(false);
  });
});

describe('transformHeaderConfigs', () => {
  it('returns an empty array when headers is undefined', () => {
    expect(transformHeaderConfigs(undefined)).toEqual([]);
  });

  it('maps "value" headers to a server value header', () => {
    const headers: ClientHeader[] = [
      { name: 'x-foo', value: 'bar', type: 'value' },
    ];
    expect(transformHeaderConfigs(headers)).toEqual([
      { name: 'x-foo', value: 'bar' },
    ]);
  });

  it('maps "env" headers to a server value_from_env header', () => {
    const headers: ClientHeader[] = [
      { name: 'x-foo', value: 'MY_ENV', type: 'env' },
    ];
    expect(transformHeaderConfigs(headers)).toEqual([
      { name: 'x-foo', value_from_env: 'MY_ENV' },
    ]);
  });

  it('filters out headers whose value is falsy (empty string)', () => {
    const headers: ClientHeader[] = [
      { name: 'x-keep', value: 'bar', type: 'value' },
      { name: 'x-drop', value: '', type: 'value' },
      { name: 'x-drop-env', value: '', type: 'env' },
    ];
    expect(transformHeaderConfigs(headers)).toEqual([
      { name: 'x-keep', value: 'bar' },
    ]);
  });
});

describe('parseHeaderConfigs', () => {
  it('returns an empty array when headers is undefined', () => {
    expect(parseHeaderConfigs(undefined)).toEqual([]);
  });

  it('parses a value header into a "value" client header', () => {
    const headers: HeaderConfig[] = [{ name: 'x-foo', value: 'bar' }];
    expect(parseHeaderConfigs(headers)).toEqual([
      { name: 'x-foo', value: 'bar', type: 'value' },
    ]);
  });

  it('parses an env header into an "env" client header', () => {
    const headers: HeaderConfig[] = [
      { name: 'x-foo', value_from_env: 'MY_ENV' },
    ];
    expect(parseHeaderConfigs(headers)).toEqual([
      { name: 'x-foo', value: 'MY_ENV', type: 'env' },
    ]);
  });

  it('does NOT filter falsy values (asymmetric with transformHeaderConfigs)', () => {
    // An env header with an empty value_from_env is still emitted, defaulting
    // to ''. This is the documented asymmetry vs transformHeaderConfigs.
    const headers: HeaderConfig[] = [
      { name: 'x-empty', value: '' },
      { name: 'x-empty-env', value_from_env: '' },
    ];
    expect(parseHeaderConfigs(headers)).toEqual([
      { name: 'x-empty', value: '', type: 'value' },
      { name: 'x-empty-env', value: '', type: 'env' },
    ]);
  });

  it('round-trips value/env headers back through transformHeaderConfigs', () => {
    const server: HeaderConfig[] = [
      { name: 'x-foo', value: 'bar' },
      { name: 'x-baz', value_from_env: 'MY_ENV' },
    ];
    expect(transformHeaderConfigs(parseHeaderConfigs(server))).toEqual(server);
  });
});
