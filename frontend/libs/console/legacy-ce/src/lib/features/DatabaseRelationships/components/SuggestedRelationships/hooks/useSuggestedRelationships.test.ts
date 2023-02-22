import { Table } from '../../../../hasura-metadata-types';
import {
  addConstraintName,
  filterTableRelationships,
  removeExistingRelationships,
} from './useSuggestedRelationships';
import { LocalRelationship, SuggestedRelationship } from '../../../types';

describe('filterTableRelationships', () => {
  it('filters relationships', () => {
    const table: Table = ['Album'];
    const relationships: SuggestedRelationship[] = [
      {
        type: 'object',
        from: {
          table: ['Album'],
          columns: ['ArtistId'],
        },
        to: {
          table: ['Artist'],
          columns: ['ArtistId'],
        },
      },
      {
        type: 'object',
        from: {
          table: ['Genre'],
          columns: ['GenreId'],
        },
        to: {
          table: ['Artist'],
          columns: ['GenreId'],
        },
      },
    ];

    expect(filterTableRelationships({ table, relationships })).toEqual([
      relationships[0],
    ]);
  });
});

describe('removeExistingRelationships', () => {
  it('removes existing relationships', () => {
    const relationships: SuggestedRelationship[] = [
      {
        type: 'object',
        from: {
          table: ['Album'],
          columns: ['ArtistId'],
        },
        to: {
          table: ['Artist'],
          columns: ['ArtistId'],
        },
      },
      {
        type: 'object',
        from: {
          table: ['Genre'],
          columns: ['GenreId'],
        },
        to: {
          table: ['Artist'],
          columns: ['GenreId'],
        },
      },
    ];
    const existingRelationships: LocalRelationship[] = [
      {
        type: 'localRelationship',
        relationshipType: 'Object',
        definition: {
          toTable: ['Artist'],
          mapping: {
            GenreId: 'GenreId',
          },
        },
        fromSource: 'dataSource',
        fromTable: ['Genre'],
        name: 'aName',
      },
    ];

    expect(
      removeExistingRelationships({
        relationships,
        existingRelationships,
      })
    ).toEqual([
      {
        type: 'object',
        from: {
          table: ['Album'],
          columns: ['ArtistId'],
        },
        to: {
          table: ['Artist'],
          columns: ['ArtistId'],
        },
      },
    ]);
  });
});

describe('addConstraintName', () => {
  it('adds the constraint name', () => {
    const relationships: SuggestedRelationship[] = [
      {
        type: 'object',
        from: {
          table: ['Album'],
          columns: ['ArtistId'],
        },
        to: {
          table: ['Artist'],
          columns: ['ArtistId'],
        },
      },
      {
        type: 'array',
        from: {
          table: ['Genre'],
          columns: ['GenreId'],
        },
        to: {
          table: ['Artist'],
          columns: ['GenreId'],
        },
      },
    ];

    const expected = [
      {
        ...relationships[0],
        constraintName: 'Album_ArtistId_Artist_ArtistId',
      },
      {
        ...relationships[1],
        constraintName: 'Genre_GenreId_Artists_GenreId',
      },
    ];

    expect(addConstraintName(relationships)).toEqual(expected);
  });
});
