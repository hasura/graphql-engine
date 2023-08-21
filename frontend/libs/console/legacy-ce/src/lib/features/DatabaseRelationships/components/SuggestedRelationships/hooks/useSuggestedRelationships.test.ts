import { Table } from '../../../../hasura-metadata-types';
import {
  addConstraintName,
  filterTableRelationships,
} from './useSuggestedRelationships';
import { SuggestedRelationship } from '../../../types';

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

describe('addConstraintName', () => {
  describe('when the naming convention is hasura-default', () => {
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
          constraintName: 'Artist',
        },
        {
          ...relationships[1],
          constraintName: 'Artists',
        },
      ];

      expect(addConstraintName(relationships, 'hasura-default')).toEqual(
        expected
      );
    });
  });

  describe('when the naming convention is graphql-default', () => {
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
          constraintName: 'artist',
        },
        {
          ...relationships[1],
          constraintName: 'artists',
        },
      ];

      expect(addConstraintName(relationships, 'graphql-default')).toEqual(
        expected
      );
    });
  });
});
