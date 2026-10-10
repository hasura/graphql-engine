import { FaTable, FaTableColumns } from 'react-icons/fa6';
import { TrackedSuggestedRelationship } from '@hasura/metadata/api';
import { Flex } from '@radix-ui/themes';
import { RelationshipIcon } from '@hasura/shared/ui';

import { getTableDisplayName } from '@hasura/shared/utils';
const DisplaySuggestedRelationship = ({
  relationship,
}: {
  relationship: TrackedSuggestedRelationship;
}) => (
  <Flex align="center" gap="2" className="text-sm text-muted">
    <FaTable />
    <span>{getTableDisplayName(relationship.fromTable)}</span>
    /
    <FaTableColumns />
    <span>{Object.keys(relationship.columnMapping ?? {}).join(' ')}</span>
    <RelationshipIcon
      type={relationship.type === 'array' ? 'one-to-many' : 'one-to-one'}
    />
    <FaTable />
    <span>{getTableDisplayName(relationship.toTable)}</span>
    /
    <FaTableColumns />
    {Object.values(relationship.columnMapping ?? {}).join(' ')}
  </Flex>
);

export default DisplaySuggestedRelationship;
