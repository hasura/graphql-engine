import { getLocalRelationshipPayload } from './getLocalRelationshipPayload';

describe('getLocalRelationshipPayload', () => {
  it('returns an object relationship request payload with constrain on the fromTable', () => {
    expect(
      getLocalRelationshipPayload({
        dataSourcePrefix: 'pg',
        dataSourceName: 'aPostgres',
        relationship: {
          fromColumnNames: ['artistId'],
          toColumnNames: ['id'],
          fromTable: ['Album'],
          name: 'Album_artistId',
          relationshipType: 'object',
          toTable: ['Artist'],
          constraintOn: 'fromTable',
        },
      })
    ).toEqual({
      type: 'pg_create_object_relationship',
      args: {
        name: 'Album_artistId',
        source: 'aPostgres',
        table: ['Album'],
        using: {
          foreign_key_constraint_on: ['artistId'],
        },
      },
    });
  });

  it('returns an array relationship request payload with constrain on the toTable', () => {
    expect(
      getLocalRelationshipPayload({
        dataSourcePrefix: 'pg',
        dataSourceName: 'aPostgres',
        relationship: {
          fromColumnNames: ['artistId'],
          toColumnNames: ['id'],
          fromTable: ['Album'],
          name: 'Album_artistId',
          relationshipType: 'array',
          toTable: ['Artist'],
          constraintOn: 'toTable',
        },
      })
    ).toEqual({
      type: 'pg_create_array_relationship',
      args: {
        name: 'Album_artistId',
        source: 'aPostgres',
        table: ['Album'],
        using: {
          foreign_key_constraint_on: { columns: ['id'], table: ['Artist'] },
        },
      },
    });
  });
});
