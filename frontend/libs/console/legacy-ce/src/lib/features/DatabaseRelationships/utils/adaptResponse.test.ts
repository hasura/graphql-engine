import {
  LocalTableArrayRelationship,
  SameTableObjectRelationship,
  Table,
} from '@hasura/shared/types';
import { LocalRelationship, SuggestedRelationship } from '../types';
import {
  adaptLocalArrayRelationshipWithFkConstraint,
  adaptLocalObjectRelationshipWithFkConstraint,
} from './adaptResponse';

describe('adaptLocalArrayRelationshipWithFkConstraint', () => {
  it('returns the correct array relationships', () => {
    const table: Table = { name: 'Album', schema: 'public' };
    const dataSourceName = 'chinook';
    const relationship: LocalTableArrayRelationship = {
      name: 'Album_Tracks',
      using: {
        foreign_key_constraint_on: {
          column: 'AlbumId',
          table: {
            name: 'Track',
            schema: 'public',
          },
        },
      },
    };
    const suggestedRelationships: SuggestedRelationship[] = [
      {
        type: 'object',
        from: {
          table: {
            schema: 'public',
            name: 'Album',
          },
          columns: ['ArtistId'],
          constraint_name: 'FK_AlbumArtistId',
        },
        to: {
          table: {
            schema: 'public',
            name: 'Artist',
          },
          columns: ['ArtistId'],
        },
      },
      {
        type: 'array',
        from: {
          table: {
            schema: 'public',
            name: 'Artist',
          },
          columns: ['ArtistId'],
        },
        to: {
          table: {
            schema: 'public',
            name: 'Album',
          },
          columns: ['ArtistId'],
          constraint_name: 'FK_AlbumArtistId',
        },
      },
      {
        type: 'object',
        from: {
          table: {
            schema: 'public',
            name: 'Track',
          },
          columns: ['AlbumId'],
          constraint_name: 'FK_TrackAlbumId',
        },
        to: {
          table: {
            schema: 'public',
            name: 'Album',
          },
          columns: ['AlbumId'],
        },
      },
      {
        type: 'array',
        from: {
          table: {
            schema: 'public',
            name: 'Album',
          },
          columns: ['AlbumId'],
        },
        to: {
          table: {
            schema: 'public',
            name: 'Track',
          },
          columns: ['AlbumId'],
          constraint_name: 'FK_TrackAlbumId',
        },
      },
    ];

    const expected: LocalRelationship = {
      name: 'Album_Tracks',
      fromSource: 'chinook',
      fromTable: { name: 'Album', schema: 'public' },
      relationshipType: 'Array',
      type: 'localRelationship',
      definition: {
        toTable: { name: 'Track', schema: 'public' },
        toColumns: ['AlbumId'],
        fromTable: { name: 'Album', schema: 'public' },
        fromColumns: ['AlbumId'],
        mapping: { AlbumId: 'AlbumId' },
      },
    };

    const result = adaptLocalArrayRelationshipWithFkConstraint({
      table,
      dataSourceName,
      relationship,
      suggestedRelationships,
    });

    console.log(result);

    expect(result).toEqual(expected);
  });

  describe('scenario two', () => {
    it('returns the correct array relationships', () => {
      const table: Table = { name: 'table_a', schema: 'public' };
      const dataSourceName = 'aPostgres';
      const relationship: LocalTableArrayRelationship = {
        name: 'table_a_table_cs',
        using: {
          foreign_key_constraint_on: {
            column: 'id_a',
            table: { name: 'table_c', schema: 'public' },
          },
        },
      };
      const suggestedRelationships: SuggestedRelationship[] = [
        {
          type: 'array',
          from: {
            table: { schema: 'public', name: 'table_a' },
            columns: ['id'],
          },
          to: {
            table: { schema: 'public', name: 'table_c' },
            columns: ['id_a'],
            constraint_name: 'table_c_id_a_fkey',
          },
        },
        {
          type: 'array',
          from: {
            table: { schema: 'public', name: 'table_b' },
            columns: ['id'],
          },
          to: {
            table: { schema: 'public', name: 'table_c' },
            columns: ['id_b'],
            constraint_name: 'table_c_id_b_fkey',
          },
        },
      ];

      const expected: LocalRelationship = {
        name: 'table_a_table_cs',
        fromSource: 'aPostgres',
        fromTable: { schema: 'public', name: 'table_a' },
        relationshipType: 'Array',
        type: 'localRelationship',
        definition: {
          toTable: { schema: 'public', name: 'table_c' },
          toColumns: ['id_a'],
          fromTable: { schema: 'public', name: 'table_a' },
          fromColumns: ['id'],
          mapping: { id_a: 'id' },
        },
      };

      const result = adaptLocalArrayRelationshipWithFkConstraint({
        table,
        dataSourceName,
        relationship,
        suggestedRelationships,
      });

      console.log(result);

      expect(result).toEqual(expected);
    });
  });

  it('takes the target table from the metadata without a matching constraint', () => {
    const relationship: LocalTableArrayRelationship = {
      name: 'table_a_table_cs',
      using: {
        foreign_key_constraint_on: {
          column: 'id_a',
          table: { name: 'table_c', schema: 'public' },
        },
      },
    };

    const result = adaptLocalArrayRelationshipWithFkConstraint({
      table: { name: 'table_a', schema: 'public' },
      dataSourceName: 'aPostgres',
      relationship,
      suggestedRelationships: [],
    });

    expect(result.fromTable).toEqual({ name: 'table_a', schema: 'public' });
    expect(result.definition).toEqual({
      toTable: { name: 'table_c', schema: 'public' },
      toColumns: ['id_a'],
      fromTable: { name: 'table_a', schema: 'public' },
      mapping: {},
    });
  });
});

describe('adaptLocalObjectRelationshipWithFkConstraint', () => {
  it('matches the constraint columns in any order without reordering the metadata', () => {
    const columns = ['b_id', 'a_id'];
    const relationship: SameTableObjectRelationship = {
      name: 'parent',
      using: { foreign_key_constraint_on: columns },
    };

    const result = adaptLocalObjectRelationshipWithFkConstraint({
      table: { name: 'child', schema: 'public' },
      dataSourceName: 'default',
      relationship,
      suggestedRelationships: [
        {
          type: 'object',
          from: {
            table: { schema: 'public', name: 'child' },
            columns: ['b_id', 'a_id'],
          },
          to: {
            table: { schema: 'public', name: 'parent' },
            columns: ['b', 'a'],
          },
        },
      ],
    });

    expect(result.definition.toTable).toEqual({
      schema: 'public',
      name: 'parent',
    });
    expect(columns).toEqual(['b_id', 'a_id']);
  });

  it('returns the correct object relationships', () => {
    const table: Table = {
      name: 'Album',
      schema: 'public',
    };
    const dataSourceName = 'chinook';

    const suggestedRelationships: SuggestedRelationship[] = [
      {
        type: 'object',
        from: {
          table: {
            schema: 'public',
            name: 'Album',
          },
          columns: ['ArtistId'],
          constraint_name: 'FK_AlbumArtistId',
        },
        to: {
          table: {
            schema: 'public',
            name: 'Artist',
          },
          columns: ['ArtistId'],
        },
      },
      {
        type: 'array',
        from: {
          table: {
            schema: 'public',
            name: 'Artist',
          },
          columns: ['ArtistId'],
        },
        to: {
          table: {
            schema: 'public',
            name: 'Album',
          },
          columns: ['ArtistId'],
          constraint_name: 'FK_AlbumArtistId',
        },
      },
      {
        type: 'object',
        from: {
          table: {
            schema: 'public',
            name: 'Track',
          },
          columns: ['AlbumId'],
          constraint_name: 'FK_TrackAlbumId',
        },
        to: {
          table: {
            schema: 'public',
            name: 'Album',
          },
          columns: ['AlbumId'],
        },
      },
      {
        type: 'array',
        from: {
          table: {
            schema: 'public',
            name: 'Album',
          },
          columns: ['AlbumId'],
        },
        to: {
          table: {
            schema: 'public',
            name: 'Track',
          },
          columns: ['AlbumId'],
          constraint_name: 'FK_TrackAlbumId',
        },
      },
    ];

    const relationship: SameTableObjectRelationship = {
      name: 'Album_Artist',
      using: {
        foreign_key_constraint_on: 'ArtistId',
      },
    };

    const expected: LocalRelationship = {
      name: 'Album_Artist',
      fromSource: 'chinook',
      fromTable: {
        name: 'Album',
        schema: 'public',
      },
      relationshipType: 'Object',
      type: 'localRelationship',
      definition: {
        toTable: {
          schema: 'public',
          name: 'Artist',
        },
        toColumns: ['ArtistId'],
        fromTable: {
          schema: 'public',
          name: 'Album',
        },
        fromColumns: ['ArtistId'],
        mapping: {
          ArtistId: 'ArtistId',
        },
      },
    };

    expect(
      adaptLocalObjectRelationshipWithFkConstraint({
        table,
        dataSourceName,
        relationship,
        suggestedRelationships,
      }),
    ).toEqual(expected);
  });
});
