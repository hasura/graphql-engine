import { getRemoteFieldPath } from '../../../../RelationshipsTable';
import React from 'react';
import {
  FaArrowRight,
  FaColumns,
  FaFont,
  FaPlug,
  FaTable,
} from 'react-icons/fa';
import { Relationship } from '../../../types';
import { getTableDisplayName } from '../../../utils/helpers';

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
    <div className="flex items-center gap-6">
      <div className="flex items-center gap-2">
        <FaTable />
        <span>{getTableDisplayName(relationship.fromTable)}</span>
        /
        <FaColumns />{' '}
        {relationship.type === 'remoteSchemaRelationship' ? (
          relationship.definition.lhs_fields.join(',')
        ) : (
          <Columns mapping={relationship.definition.mapping} type="from" />
        )}
      </div>
      <FaArrowRight />

      <div className="flex items-center gap-2">
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
            <div>{getTableDisplayName(relationship.definition.toSource)}</div>
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
      </div>
    </div>
  );
};
