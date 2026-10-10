import React from 'react';
import { Flex } from '@radix-ui/themes';
import { FaTable, FaDatabase, FaPlug } from 'react-icons/fa';
import { RelationshipSourceType } from '../types';

const getRelationIcon = (type: RelationshipSourceType) => {
  if (type === 'local_object')
    return (
      <FaTable className="fill-current text-muted text-sm mr-2" title="Table" />
    );
  if (type === 'local_array')
    return (
      <FaTable className="fill-current text-muted text-sm mr-2" title="Table" />
    );
  if (type === 'to_source')
    return (
      <FaDatabase
        className="fill-current text-muted text-sm mr-2"
        title="Database"
      />
    );
  if (type === 'to_remote_schema')
    return (
      <FaPlug
        className="fill-current text-muted text-sm mr-2"
        title="Remote schema"
      />
    );
  if (type === 'remote_schema_legacy')
    return (
      <FaPlug
        className="fill-current text-muted text-sm mr-2"
        title="Remote schema"
      />
    );
  return null;
};

type SourceColumnCellType = {
  type: RelationshipSourceType;
  name: React.ReactNode;
};

const SourceColumnCell = ({ type, name }: SourceColumnCellType) => {
  return (
    <Flex align="center">
      {getRelationIcon(type)} {name}
    </Flex>
  );
};

export default SourceColumnCell;
