import { Metadata } from '@hasura/shared/types';
import { extractModelsAndQueriesFromMetadata } from './nativeQuery';

const metadata = {
  resource_version: 1,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'chinook',
        kind: 'postgres',
        configuration: {},
        tables: [],
        logical_models: [{ name: 'AlbumModel', fields: [] }],
        native_queries: [
          {
            root_field_name: 'albumQuery',
            code: 'SELECT 1',
            returns: 'AlbumModel',
          },
        ],
      },
      {
        name: 'second',
        kind: 'postgres',
        configuration: {},
        tables: [],
        logical_models: [{ name: 'TrackModel', fields: [] }],
        native_queries: [
          {
            root_field_name: 'trackQuery',
            code: 'SELECT 2',
            returns: 'TrackModel',
          },
        ],
      },
      {
        name: 'empty',
        kind: 'postgres',
        configuration: {},
        tables: [],
      },
    ],
  },
} as unknown as Metadata;

describe('extractModelsAndQueriesFromMetadata', () => {
  it('collects logical models across all sources, tagged with their source', () => {
    const { models } = extractModelsAndQueriesFromMetadata(metadata);
    expect(models.map((m) => m.name)).toEqual(['AlbumModel', 'TrackModel']);
    expect(models[0].source.name).toBe('chinook');
    expect(models[1].source.name).toBe('second');
  });

  it('collects native queries across all sources, tagged with their source', () => {
    const { queries } = extractModelsAndQueriesFromMetadata(metadata);
    expect(queries.map((q) => q.root_field_name)).toEqual([
      'albumQuery',
      'trackQuery',
    ]);
    expect(queries[0].source.name).toBe('chinook');
  });

  it('returns empty arrays when no source has models or queries', () => {
    const empty = {
      resource_version: 1,
      metadata: { version: 3, sources: [] },
    } as unknown as Metadata;
    expect(extractModelsAndQueriesFromMetadata(empty)).toEqual({
      models: [],
      queries: [],
    });
  });
});
