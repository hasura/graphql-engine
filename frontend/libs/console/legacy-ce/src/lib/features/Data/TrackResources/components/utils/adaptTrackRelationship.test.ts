import { SuggestedRelationshipWithName } from '../../../../DatabaseRelationships/components/SuggestedRelationships/hooks/useSuggestedRelationships';
import adaptTrackRelationship from './adaptTrackRelationship';

describe('adaptTrackRelationship', () => {
  it('returns the object relationship with constrain on the from table', () => {
    const objectRelationship: SuggestedRelationshipWithName = {
      type: 'object',
      from: {
        table: ['Album'],
        columns: ['artistId'],
        constraint_name: 'Album_artistId',
      },
      to: {
        table: ['Artist'],
        columns: ['id'],
      },
      constraintName: 'Album_artistId',
    };
    expect(adaptTrackRelationship(objectRelationship)).toEqual({
      fromColumnNames: ['artistId'],
      toColumnNames: ['id'],
      fromTable: ['Album'],
      name: 'Album_artistId',
      relationshipType: 'object',
      toTable: ['Artist'],
      constraintOn: 'fromTable',
    });
  });

  it('returns the object relationship with constrain on the to table', () => {
    const objectRelationship: SuggestedRelationshipWithName = {
      type: 'object',
      from: {
        table: ['Album'],
        columns: ['artistId'],
      },
      to: {
        table: ['Artist'],
        columns: ['id'],
        constraint_name: 'Album_artistId',
      },
      constraintName: 'Album_artistId',
    };
    expect(adaptTrackRelationship(objectRelationship)).toEqual({
      fromColumnNames: ['artistId'],
      toColumnNames: ['id'],
      fromTable: ['Album'],
      name: 'Album_artistId',
      relationshipType: 'object',
      toTable: ['Artist'],
      constraintOn: 'toTable',
    });
  });

  it('returns the array relationship with constrain on the from table', () => {
    const objectRelationship: SuggestedRelationshipWithName = {
      type: 'array',
      from: {
        table: ['Album'],
        columns: ['artistId'],
        constraint_name: 'Album_artistId',
      },
      to: {
        table: ['Artist'],
        columns: ['id'],
      },
      constraintName: 'Album_artistId',
    };
    expect(adaptTrackRelationship(objectRelationship)).toEqual({
      fromColumnNames: ['artistId'],
      toColumnNames: ['id'],
      fromTable: ['Album'],
      name: 'Album_artistId',
      relationshipType: 'array',
      toTable: ['Artist'],
      constraintOn: 'fromTable',
    });
  });

  it('returns the array relationship with constrain on the to table', () => {
    const objectRelationship: SuggestedRelationshipWithName = {
      type: 'array',
      from: {
        table: ['Album'],
        columns: ['artistId'],
      },
      to: {
        table: ['Artist'],
        columns: ['id'],
        constraint_name: 'Album_artistId',
      },
      constraintName: 'Album_artistId',
    };
    expect(adaptTrackRelationship(objectRelationship)).toEqual({
      fromColumnNames: ['artistId'],
      toColumnNames: ['id'],
      fromTable: ['Album'],
      name: 'Album_artistId',
      relationshipType: 'array',
      toTable: ['Artist'],
      constraintOn: 'toTable',
    });
  });
});
