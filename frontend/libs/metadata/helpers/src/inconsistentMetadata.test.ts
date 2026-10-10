import {
  InconsistentObject,
  InconsistentObjectRemoteSchema,
  InconsistentSource,
} from '@hasura/shared/types';
import {
  findInconsistentRemoteSchema,
  findInconsistentSource,
} from './inconsistentMetadata';

const remoteSchemaInconsistency: InconsistentObjectRemoteSchema = {
  type: 'remote_schema',
  name: 'remote_schema my_remote',
  reason: 'Inconsistent object: remote schema is unreachable',
  definition: {
    name: 'my_remote',
    definition: { url: 'http://example.com' },
  },
};

const sourceInconsistency: InconsistentSource = {
  type: 'source',
  name: 'source my_source',
  reason: 'Inconsistent object: connection failed',
  definition: 'my_source',
};

describe('findInconsistentRemoteSchema', () => {
  it('matches on the literal "remote_schema ${name}" prefix', () => {
    const result = findInconsistentRemoteSchema(
      [remoteSchemaInconsistency],
      'my_remote',
    );
    expect(result).toBe(remoteSchemaInconsistency);
  });

  it('does NOT match when passed a name that already includes the prefix', () => {
    // Documented gotcha: the match is a literal prefix comparison, so passing
    // the already-prefixed name fails to match.
    const result = findInconsistentRemoteSchema(
      [remoteSchemaInconsistency],
      'remote_schema my_remote',
    );
    expect(result).toBeUndefined();
  });

  it('returns undefined when the type is not remote_schema', () => {
    const result = findInconsistentRemoteSchema(
      [sourceInconsistency as unknown as InconsistentObject],
      'my_source',
    );
    expect(result).toBeUndefined();
  });

  it('returns undefined when there is no matching name', () => {
    expect(
      findInconsistentRemoteSchema([remoteSchemaInconsistency], 'other'),
    ).toBeUndefined();
  });

  it('returns undefined when the input list is undefined', () => {
    expect(
      findInconsistentRemoteSchema(undefined, 'my_remote'),
    ).toBeUndefined();
  });

  it('ignores entries without a "type" field', () => {
    const grouped = {
      objects: [],
      reason: 'grouped inconsistency',
    } as InconsistentObject;
    expect(
      findInconsistentRemoteSchema([grouped], 'my_remote'),
    ).toBeUndefined();
  });
});

describe('findInconsistentSource', () => {
  it('matches a source by exact definition string', () => {
    const result = findInconsistentSource([sourceInconsistency], 'my_source');
    expect(result).toBe(sourceInconsistency);
  });

  it('returns undefined when the definition does not match', () => {
    expect(
      findInconsistentSource([sourceInconsistency], 'other_source'),
    ).toBeUndefined();
  });

  it('returns undefined when the type is not source', () => {
    expect(
      findInconsistentSource(
        [remoteSchemaInconsistency as unknown as InconsistentObject],
        'my_remote',
      ),
    ).toBeUndefined();
  });

  it('returns undefined for an empty list', () => {
    expect(findInconsistentSource([], 'my_source')).toBeUndefined();
  });
});
