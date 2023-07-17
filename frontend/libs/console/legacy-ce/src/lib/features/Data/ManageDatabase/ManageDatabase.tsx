import get from 'lodash/get';
import { Analytics, REDACT_EVERYTHING } from '../../Analytics';
import { ManageTrackedTables } from '../ManageTable/components/ManageTrackedTables';
import { ManageTrackedFunctions } from '../TrackResources/TrackFunctions/components/ManageTrackedFunctions';
import { ManageTrackedRelationshipsContainer } from '../TrackResources/components/ManageTrackedRelationshipsContainer';
import { useDriverCapabilities } from '../hooks/useDriverCapabilities';
import { BreadCrumbs, CollapsibleResource, SourceName } from './parts';

export interface ManageDatabaseProps {
  dataSourceName: string;
  schema?: string;
}

//This component has the code for template gallery but is currently commented out until further notice.
export const ManageDatabase = ({ dataSourceName }: ManageDatabaseProps) => {
  const {
    data: {
      areForeignKeysSupported = false,
      areUserDefinedFunctionsSupported = false,
    } = {},
  } = useDriverCapabilities({
    dataSourceName,
    select: data => {
      return {
        areForeignKeysSupported: !!get(
          data,
          'data_schema.supports_foreign_keys'
        ),
        areUserDefinedFunctionsSupported: !!get(data, 'user_defined_functions'),
      };
    },
  });

  return (
    <Analytics name="ManageDatabaseV2" {...REDACT_EVERYTHING}>
      <div className="w-full overflow-y-auto bg-gray-50">
        <div className="px-md pt-md mb-xs">
          <BreadCrumbs dataSourceName={dataSourceName} />
          <SourceName dataSourceName={dataSourceName} />
        </div>
        <div className="px-md group relative gap-2 flex-col flex">
          <CollapsibleResource
            title="Tables/Views"
            tooltip="Expose the tables available in your database via the GraphQL API"
            defaultOpen
          >
            <ManageTrackedTables
              dataSourceName={dataSourceName}
              key={dataSourceName}
            />
          </CollapsibleResource>

          {areForeignKeysSupported && (
            <CollapsibleResource
              title="Foreign Key Relationships"
              tooltip="Track foreign key relationships in your database in your GraphQL API"
            >
              <ManageTrackedRelationshipsContainer
                dataSourceName={dataSourceName}
              />
            </CollapsibleResource>
          )}

          {areUserDefinedFunctionsSupported && (
            <CollapsibleResource
              title="Untracked Custom Functions"
              tooltip="Expose the functions available in your database via the GraphQL API"
            >
              <ManageTrackedFunctions dataSourceName={dataSourceName} />
            </CollapsibleResource>
          )}
        </div>
      </div>
    </Analytics>
  );
};
