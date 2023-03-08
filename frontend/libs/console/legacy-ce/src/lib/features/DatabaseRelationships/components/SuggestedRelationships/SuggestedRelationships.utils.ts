import { capitaliseFirstLetter } from '../../../../components/Common/ConfigureTransformation/utils';
import { LocalRelationship } from '../../types';
import { SuggestedRelationshipWithName } from './hooks/useSuggestedRelationships';

export const convertSuggestedRelationShipToLocalRelationship = (
  dataSourceName: string,
  suggestedRelationship: SuggestedRelationshipWithName
): LocalRelationship => ({
  definition: {
    toTable: suggestedRelationship.to.table,
    mapping: suggestedRelationship.to.columns.reduce((acc, column, index) => {
      return { ...acc, [suggestedRelationship.from.columns[index]]: column };
    }, {}),
  },
  fromSource: dataSourceName,
  fromTable: suggestedRelationship.from.table,
  relationshipType: capitaliseFirstLetter(
    suggestedRelationship.type
  ) as LocalRelationship['relationshipType'],
  type: 'localRelationship',
  name: suggestedRelationship.constraintName,
});
