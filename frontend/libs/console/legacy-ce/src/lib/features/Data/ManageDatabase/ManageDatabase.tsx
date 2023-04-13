import { Analytics, REDACT_EVERYTHING } from '../../Analytics';
import { FaAngleRight, FaDatabase } from 'react-icons/fa';
import { ManageTrackedTables } from '../ManageTable/components/ManageTrackedTables';
import { ManageTrackedRelationshipsContainer } from '../TrackResources/components/ManageTrackedRelationshipsContainer';

interface ManageDatabaseProps {
  dataSourceName: string;
}

export const ManageDatabase = (props: ManageDatabaseProps) => {
  return (
    <Analytics name="ManageDatabaseV2" {...REDACT_EVERYTHING}>
      <div className="w-full overflow-y-auto bg-gray-50">
        <div className="px-md pt-md mb-xs">
          <div className="flex items-center space-x-xs mb-1">
            <div className="cursor-pointer flex items-center text-muted hover:text-gray-900">
              <FaDatabase className="mr-1.5" />
              <span className="text-sm">{props.dataSourceName}</span>
            </div>
            <FaAngleRight className="text-muted" />
            <div className="cursor-pointer flex items-center">
              <span className="text-sm font-semibold text-yellow-500">
                Manage
              </span>
            </div>
          </div>

          <div className="flex items-center">
            <div className="group relative">
              <h1 className="inline-flex items-center text-xl font-semibold mb-1">
                {props.dataSourceName}
              </h1>
            </div>
          </div>
        </div>
        <div>
          <div className="px-md group relative">
            <ManageTrackedTables dataSourceName={props.dataSourceName} />
            <ManageTrackedRelationshipsContainer
              dataSourceName={props.dataSourceName}
            />
          </div>
        </div>
      </div>
    </Analytics>
  );
};
