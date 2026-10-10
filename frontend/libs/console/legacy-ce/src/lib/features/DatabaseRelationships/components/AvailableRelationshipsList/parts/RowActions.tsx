import { FaEdit, FaPencilAlt, FaTrash } from 'react-icons/fa';
import { MODE, Relationship } from '../../../types';
import { Flex } from '@radix-ui/themes';
import { Button } from '@hasura/shared/ui';

export const RowActions = ({
  relationship,
  onActionClick,
}: {
  relationship: Relationship;
  onActionClick: (relationship: Relationship, mode: MODE) => void;
}) => {
  return (
    <Flex align="center" justify="end" className="whitespace-nowrap" gap="2">
      {relationship.type === 'localRelationship' && (
        <Button
          size="1"
          mode="default"
          leftIcon={FaPencilAlt}
          onClick={() => onActionClick(relationship, MODE.RENAME)}
        >
          Rename
        </Button>
      )}

      {['remoteSchemaRelationship', 'remoteDatabaseRelationship'].includes(
        relationship.type,
      ) && (
        <Button
          mode="default"
          size="1"
          leftIcon={FaEdit}
          onClick={() => onActionClick(relationship, MODE.EDIT)}
        >
          <FaEdit className="fill-current mr-1" />
          Edit
        </Button>
      )}

      <Button
        mode="destructive"
        size="1"
        leftIcon={FaTrash}
        onClick={() => onActionClick(relationship, MODE.DELETE)}
      >
        Remove
      </Button>
    </Flex>
  );
};
