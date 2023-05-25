import React from 'react';
import { FaAngleRight, FaDatabase } from 'react-icons/fa';
import { FunctionDisplayName } from '../../TrackResources/TrackFunctions/components/FunctionDisplayName';
import { QualifiedFunction } from '../../../hasura-metadata-types';

export const Breadcrumbs: React.VFC<{
  dataSourceName: string;
  qualifiedFunction: QualifiedFunction;
}> = ({ dataSourceName, qualifiedFunction }) => (
  <div className="flex items-center space-x-xs mb-4">
    <div className="cursor-pointer flex items-center text-muted hover:text-gray-900">
      <FaDatabase className="mr-1.5" />
      <span className="text-sm">{dataSourceName}</span>
    </div>
    <FaAngleRight className="text-muted" />
    <div className="cursor-pointer flex items-center text-muted hover:text-gray-900">
      <FunctionDisplayName qualifiedFunction={qualifiedFunction} />
    </div>
    <FaAngleRight className="text-muted" />
    <div className="cursor-pointer flex items-center">
      <span className="text-sm font-semibold text-yellow-500">Manage</span>
    </div>
  </div>
);
