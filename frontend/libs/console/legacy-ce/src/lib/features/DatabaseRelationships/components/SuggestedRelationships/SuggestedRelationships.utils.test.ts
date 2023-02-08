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
        columns: ['ArtistId', 'ArtistNameId'],
      },
      to: {
        table: ['Artist'],
        columns: ['Id', 'NameId'],
      },
    };

    const expected = {
      definition: {
        mapping: {
          ArtistId: 'Id',
          ArtistNameId: 'NameId',
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
