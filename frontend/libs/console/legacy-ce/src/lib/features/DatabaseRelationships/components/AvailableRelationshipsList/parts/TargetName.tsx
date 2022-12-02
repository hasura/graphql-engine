import React from 'react';
import { FaDatabase, FaPlug } from 'react-icons/fa';
import { Relationship } from '../../../types';

export const TargetName = ({
  relationship,
}: {
  relationship: Relationship;
}) => {
  if (relationship.type === 'remoteSchemaRelationship')
    return (
      <>
        <FaPlug /> <span>{relationship.definition.toRemoteSchema}</span>
      </>
    );

  if (relationship.type === 'remoteDatabaseRelationship')
    return (
      <>
        <FaDatabase /> <span>{relationship.definition.toSource}</span>
      </>
    );

  return (
    <>
      <FaDatabase /> <span>{relationship.fromSource}</span>
    </>
  );
};
