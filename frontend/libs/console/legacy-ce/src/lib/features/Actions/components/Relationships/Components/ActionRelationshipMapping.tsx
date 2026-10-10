import { FaColumns, FaTable } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';
import { getTableDisplayName } from '@hasura/shared/utils';
import { RelationshipIcon } from '@hasura/shared/ui';
import { CustomTypeObjectRelationship } from '@hasura/shared/types';

const Columns = ({
  mapping,
  type,
}: {
  mapping: Record<string, string>;
  type: 'from' | 'to';
}) => {
  const isMappingPresent = Object.entries(mapping)?.length ?? undefined;

  if (!isMappingPresent) {
    return <></>;
  }

  return type === 'from' ? (
    <>{Object.keys(mapping).join(',')}</>
  ) : (
    <>{Object.values(mapping).join(',')}</>
  );
};

const ActionRelationshipMapping = ({
  typeName,
  relationship,
}: {
  typeName: string;
  relationship: CustomTypeObjectRelationship;
}) => {
  return (
    <Flex align="center" gap="6">
      <Flex align="center" gap="2">
        <span>{typeName}</span>
        /
        <FaColumns />{' '}
        <Columns mapping={relationship.field_mapping} type="from" />
      </Flex>
      <RelationshipIcon
        type={
          relationship.type === 'array'
            ? 'one-to-many'
            : relationship.type === 'object'
              ? 'one-to-one'
              : 'other'
        }
      />
      <Flex align="center" gap="2">
        <FaTable />
        <div>{getTableDisplayName(relationship.remote_table)}</div>
        /
        <FaColumns />
        <Columns mapping={relationship.field_mapping} type="to" />
      </Flex>
    </Flex>
  );
};

export default ActionRelationshipMapping;
