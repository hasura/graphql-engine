import React, { ReactText } from 'react';
import { FaTable, FaDatabase, FaPlug } from 'react-icons/fa';
import { RelationshipSourceType } from '../types';

const getRelationIcon = (type: RelationshipSourceType) => {
  if (type === 'local_object')
    return <FaTable className="fill-current text-muted text-sm mr-2" />;
  if (type === 'local_array')
    return <FaTable className="fill-current text-muted text-sm mr-2" />;
  if (type === 'to_source')
    return <FaDatabase className="fill-current text-muted text-sm mr-2" />;
  if (type === 'to_remote_schema')
    return <FaPlug className="fill-current text-muted text-sm mr-2" />;
  if (type === 'remote_schema_legacy')
    return <FaPlug className="fill-current text-muted text-sm mr-2" />;
};

type SourceColumnCellType = {
  type: RelationshipSourceType;
  name: ReactText;
};

const SourceColumnCell = ({ type, name }: SourceColumnCellType) => {
  return (
    <>
      <div className="flex items-center">
        {getRelationIcon(type)} {name}
      </div>
    </>
  );
};

export default SourceColumnCell;
