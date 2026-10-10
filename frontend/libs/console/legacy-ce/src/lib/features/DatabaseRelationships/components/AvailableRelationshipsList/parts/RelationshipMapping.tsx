import { FaColumns, FaFont, FaPlug, FaTable } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';
import { getRemoteFieldPath } from '../../../../RelationshipsTable';
import { Relationship } from '../../../types';
import { getTableDisplayName } from '@hasura/shared/utils';
import { RelationshipIcon } from '@hasura/shared/ui';

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

export const RelationshipMapping = ({
  relationship,
}: {
  relationship: Relationship;
}) => {
  if (relationship.type !== 'remoteSchemaRelationship') {
    const isMappingPresent =
      Object.entries(relationship.definition?.mapping)?.length ?? undefined;

    if (!isMappingPresent) {
      return null;
    }
  }

  return (
    <Flex align="center" gap="6">
      <Flex align="center" gap="2">
        <FaTable />
        <span>{getTableDisplayName(relationship.fromTable)}</span>
        /
        <FaColumns />{' '}
        {relationship.type === 'remoteSchemaRelationship' ? (
          relationship.definition.lhs_fields.join(',')
        ) : (
          <Columns mapping={relationship.definition.mapping} type="from" />
        )}
      </Flex>
      <RelationshipIcon
        type={
          relationship.relationshipType === 'Array'
            ? 'one-to-many'
            : relationship.relationshipType === 'Object'
              ? 'one-to-one'
              : 'other'
        }
      />
      <Flex align="center" gap="2">
        {relationship.type === 'remoteSchemaRelationship' ? (
          <>
            <FaPlug />
            <div>{relationship.definition.toRemoteSchema}</div> /
            <FaFont />{' '}
            {getRemoteFieldPath(relationship.definition.remote_field)}
          </>
        ) : relationship.type === 'remoteDatabaseRelationship' ? (
          <>
            <FaTable />
            <div>{getTableDisplayName(relationship.definition.toTable)}</div>
            /
            <FaColumns />
            <Columns mapping={relationship.definition.mapping} type="to" />
          </>
        ) : (
          <>
            <FaTable />
            <div>{getTableDisplayName(relationship.definition.toTable)}</div>
            /
            <FaColumns />
            <Columns mapping={relationship.definition.mapping} type="to" />
          </>
        )}
      </Flex>
    </Flex>
  );
};
