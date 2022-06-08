import React from 'react';
import { IconType } from 'react-icons';
import { FaColumns, FaDatabase, FaFont, FaPlug, FaTable } from 'react-icons/fa';
import { FiType } from 'react-icons/fi';

const legend: { Icon: IconType; name: string }[] = [
  {
    Icon: FaPlug,
    name: 'Remote Schema',
  },
  {
    Icon: FiType,
    name: 'Type',
  },
  {
    Icon: FaFont,
    name: 'Field',
  },
  {
    Icon: FaDatabase,
    name: 'Database',
  },
  {
    Icon: FaTable,
    name: 'Table',
  },
  {
    Icon: FaColumns,
    name: 'Column',
  },
];

const Legends = () => {
  return (
    <div className="text-right mb-4">
      {legend.map(item => {
        const { Icon, name } = item;
        return (
          <span>
            <Icon className="mr-1 ml-4 text-sm" style={{ strokeWidth: 4.5 }} />
            {name}
          </span>
        );
      })}
    </div>
  );
};

export default Legends;
