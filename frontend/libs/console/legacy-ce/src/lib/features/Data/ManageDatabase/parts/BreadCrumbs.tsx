import { FaAngleRight, FaDatabase } from 'react-icons/fa';
import { ManageDatabaseProps } from '../ManageDatabase';

export function BreadCrumbs({ dataSourceName }: ManageDatabaseProps) {
  return (
    <div className="flex items-center space-x-xs mb-1">
      <div className="cursor-pointer flex items-center text-muted hover:text-gray-900">
        <FaDatabase className="mr-1.5" />
        <span className="text-sm">{dataSourceName}</span>
      </div>
      <FaAngleRight className="text-muted" />
      <div className="cursor-pointer flex items-center">
        <span className="text-sm font-semibold text-yellow-500">Manage</span>
      </div>
    </div>
  );
}
