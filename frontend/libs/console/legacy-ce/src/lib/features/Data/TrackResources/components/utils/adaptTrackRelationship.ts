import { AddSuggestedRelationship } from '../../../../DatabaseRelationships/components/SuggestedRelationships/hooks/useAllSuggestedRelationships';
import { SuggestedRelationshipWithName } from '../../../../DatabaseRelationships/components/SuggestedRelationships/hooks/useSuggestedRelationships';

const adaptTrackRelationship = (
  relationship: SuggestedRelationshipWithName
): AddSuggestedRelationship => {
  const isConstraintOnFromTable = !!relationship.from?.constraint_name;
  const isObjectRelationship = relationship.type === 'object';
  return {
    name: relationship.constraintName,
    fromColumnNames: relationship.from.columns,
    toColumnNames: relationship.to.columns,
    relationshipType: isObjectRelationship ? 'object' : 'array',
    toTable: relationship.to.table,
    fromTable: relationship.from.table,
    constraintOn: isConstraintOnFromTable ? 'fromTable' : 'toTable',
  };
};

export default adaptTrackRelationship;
