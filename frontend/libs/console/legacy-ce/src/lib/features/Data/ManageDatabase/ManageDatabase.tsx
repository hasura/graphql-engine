import { Analytics, REDACT_EVERYTHING } from '../../Analytics';
import { ManageTrackedRelationshipsContainer } from '../TrackResources/components/ManageTrackedRelationshipsContainer';
import { ManageTrackedTables } from '../ManageTable/components/ManageTrackedTables';
import { BreadCrumbs, SourceName } from './components';
export interface ManageDatabaseProps {
  dataSourceName: string;
}

export const ManageDatabase = ({ dataSourceName }: ManageDatabaseProps) => {
  return (
    <Analytics name="ManageDatabaseV2" {...REDACT_EVERYTHING}>
      <div className="w-full overflow-y-auto bg-gray-50">
        <div className="px-md pt-md mb-xs">
          <BreadCrumbs dataSourceName={dataSourceName} />
          <SourceName dataSourceName={dataSourceName} />
        </div>
        <div className="px-md group relative">
          <ManageTrackedTables dataSourceName={dataSourceName} />
          <ManageTrackedRelationshipsContainer
            dataSourceName={dataSourceName}
          />
        </div>
      </div>
    </Analytics>
  );
};
