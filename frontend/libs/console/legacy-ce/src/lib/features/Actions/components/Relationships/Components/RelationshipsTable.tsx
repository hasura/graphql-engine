import { FaEdit, FaTrash } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';
import { Button, CardedTable, IndicatorCard } from '@hasura/shared/ui';
import { CustomTypeObjectRelationship } from '@hasura/shared/types';
import ActionRelationshipMapping from './ActionRelationshipMapping';

type Props = {
  typeName: string;
  relationships: CustomTypeObjectRelationship[];
  readOnlyMode: boolean;
  onEdit: (relationship: CustomTypeObjectRelationship) => void;
  onRemove: (relationship: CustomTypeObjectRelationship) => void;
};

const RelationshipsTable = ({
  typeName,
  relationships,
  readOnlyMode,
  onEdit,
  onRemove,
}: Props) => {
  if (!relationships.length) {
    return (
      <IndicatorCard status="info" headline="No relationships found.">
        No relationships have been added for this type yet.
      </IndicatorCard>
    );
  }

  return (
    <CardedTable
      columns={['NAME', 'TYPE', 'SOURCE', 'RELATIONSHIP', '']}
      data={relationships.map((relationship) => [
        relationship.name,
        <Flex align="center" gap="2" key={`${relationship.name}-type`}>
          {relationship.type}
        </Flex>,
        relationship.source,
        <ActionRelationshipMapping
          key={`${relationship.name}-mapping`}
          typeName={typeName}
          relationship={relationship}
        />,
        <Flex
          align="center"
          justify="end"
          gap="2"
          className="whitespace-nowrap text-right"
          key={`${relationship.name}-actions`}
        >
          {!readOnlyMode && (
            <>
              <Button
                mode="primary"
                size="sm"
                leftIcon={FaEdit}
                onClick={() => onEdit(relationship)}
              >
                Edit
              </Button>
              <Button
                mode="destructive"
                size="sm"
                leftIcon={FaTrash}
                onClick={() => onRemove(relationship)}
              >
                Remove
              </Button>
            </>
          )}
        </Flex>,
      ])}
    />
  );
};

export default RelationshipsTable;
