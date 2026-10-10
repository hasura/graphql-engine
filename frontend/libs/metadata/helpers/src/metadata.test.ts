import { Metadata } from '@hasura/shared/types';
import { isMetadataEmpty } from './metadata';

type MetadataDocument = Metadata['metadata'];

const emptySource = (name: string): MetadataDocument['sources'][number] =>
  ({
    name,
    kind: 'postgres',
    tables: [],
    configuration: {},
  }) as unknown as MetadataDocument['sources'][number];

const sourceWithTable = (name: string): MetadataDocument['sources'][number] =>
  ({
    name,
    kind: 'postgres',
    tables: [
      { table: { schema: 'public', name: 'users' }, event_triggers: [] },
    ],
    configuration: {},
  }) as unknown as MetadataDocument['sources'][number];

const baseMetadata = (
  overrides: Partial<MetadataDocument> = {},
): MetadataDocument =>
  ({
    version: 3,
    sources: [],
    ...overrides,
  }) as MetadataDocument;

describe('isMetadataEmpty', () => {
  it('returns true when there are no sources, actions or remote schemas', () => {
    expect(isMetadataEmpty(baseMetadata())).toBe(true);
  });

  it('returns true when sources exist but none have tables', () => {
    expect(
      isMetadataEmpty(baseMetadata({ sources: [emptySource('pg')] })),
    ).toBe(true);
  });

  it('returns false when at least one source has a non-empty tables array', () => {
    expect(
      isMetadataEmpty(
        baseMetadata({ sources: [emptySource('a'), sourceWithTable('b')] }),
      ),
    ).toBe(false);
  });

  it('returns false when there is at least one action', () => {
    expect(
      isMetadataEmpty(
        baseMetadata({
          actions: [{ name: 'insertUser' }] as MetadataDocument['actions'],
        }),
      ),
    ).toBe(false);
  });

  it('returns false when there is at least one remote schema', () => {
    expect(
      isMetadataEmpty(
        baseMetadata({
          remote_schemas: [
            {
              name: 'my_remote',
              definition: { url: 'http://example.com' },
            },
          ] as MetadataDocument['remote_schemas'],
        }),
      ),
    ).toBe(false);
  });

  it('returns true when actions and remote_schemas are present but empty', () => {
    expect(
      isMetadataEmpty(baseMetadata({ actions: [], remote_schemas: [] })),
    ).toBe(true);
  });
});
