import { TMigration } from '../../../../MetadataAPI';
import { AddSuggestedRelationship } from '../hooks/useAllSuggestedRelationships';

type GetRelationshipPayloadArgs = {
  dataSourcePrefix: string;
  dataSourceName: string;
  relationship: AddSuggestedRelationship;
};

export type LocalRelationshipQuery = {
  table: unknown;
  name: string;
  source: string;
  using: {
    foreign_key_constraint_on: string[] | { table: unknown; columns: string[] };
  };
};

export const getLocalRelationshipPayload = ({
  dataSourcePrefix,
  dataSourceName,
  relationship,
}: GetRelationshipPayloadArgs): TMigration<LocalRelationshipQuery>['query'] => {
  return {
    type: `${dataSourcePrefix}_create_${relationship.relationshipType}_relationship`,
    args: {
      table: relationship.fromTable,
      name: relationship.name,
      source: dataSourceName,
      using: {
        foreign_key_constraint_on:
          relationship.constraintOn === 'fromTable'
            ? relationship.fromColumnNames
            : {
                table: relationship.toTable,
                columns: relationship.toColumnNames,
              },
      },
    },
  };
};
