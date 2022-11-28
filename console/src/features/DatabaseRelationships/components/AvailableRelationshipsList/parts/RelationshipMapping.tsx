import { Tooltip } from '@/new-components/Tooltip';
import React from 'react';
import {
  FaArrowRight,
  FaColumns,
  FaFont,
  FaExclamationTriangle,
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

  return isMappingPresent ? (
    <>
      {type === 'from'
        ? Object.keys(mapping).join(',')
        : Object.values(mapping).join(',')}
    </>
  ) : (
    <Tooltip tooltipContentChildren="Unable to retrieve any column info. Please check if your datasource is reachable.">
      <FaExclamationTriangle className="text-red-600" />
    </Tooltip>
  );
};

export const RelationshipMapping = ({
  relationship,
}: {
  relationship: Relationship;
}) => {
  return (
    <div className="flex items-center gap-6">
      <div className="flex items-center gap-2">
        <FaTable />
        <span>{getTableDisplayName(relationship.fromTable)}</span>
        /
        <FaColumns />{' '}
        <Columns mapping={relationship.definition.mapping} type="from" />
      </div>
      <FaArrowRight />

      <div className="flex items-center gap-2">
        {relationship.type === 'remoteSchemaRelationship' ? (
          <>
            <FaPlug />
            <div>{relationship.definition.toRemoteSchema}</div> /
            <FaFont />{' '}
            <Columns mapping={relationship.definition.mapping} type="to" />
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
