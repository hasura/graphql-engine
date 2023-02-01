import { SuggestedRelationshipWithName } from './hooks/useSuggestedRelationships';
import { convertSuggestedRelationShipToLocalRelationship } from './SuggestedRelationships.utils';

describe('convertSuggestedRelationShipToLocalRelationship', () => {
  it('converts relationships', () => {
    const dataSourceName = 'aDataSourceName';
    const suggestedRelationship: SuggestedRelationshipWithName = {
      constraintName: 'Album_Artist_ArtistId',
      type: 'object',
      from: {
        table: ['Album'],
        columns: ['ArtistId'],
      },
      to: {
        table: ['Artist'],
        columns: ['ArtistId'],
      },
    };

    const expected = {
      definition: {
        mapping: {
          ArtistId: 'ArtistId',
        },
        toTable: ['Artist'],
      },
      fromSource: 'aDataSourceName',
      fromTable: ['Album'],
      name: 'Album_Artist_ArtistId',
      relationshipType: 'Object',
      type: 'localRelationship',
    };

    expect(
      convertSuggestedRelationShipToLocalRelationship(
        dataSourceName,
        suggestedRelationship
      )
    ).toEqual(expected);
  });
});
