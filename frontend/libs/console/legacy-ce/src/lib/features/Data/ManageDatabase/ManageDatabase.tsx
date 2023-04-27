import { Analytics, REDACT_EVERYTHING } from '../../Analytics';
//import { MetadataSelectors, useMetadata } from '../../hasura-metadata-api';
import { ManageTrackedTables } from '../ManageTable/components/ManageTrackedTables';
import { ManageTrackedRelationshipsContainer } from '../TrackResources/components/ManageTrackedRelationshipsContainer';
//import TemplateGallery from '../../../components/Services/Data/Schema/TemplateGallery/TemplateGallery';
import { BreadCrumbs, SourceName, CollapsibleResource } from './parts';

export interface ManageDatabaseProps {
  dataSourceName: string;
}

//This component has the code for template gallery but is currently commented out until further notice.
export const ManageDatabase = ({ dataSourceName }: ManageDatabaseProps) => {
  // const { data: source } = useMetadata(
  //   MetadataSelectors.findSource(dataSourceName)
  // );
  return (
    <Analytics name="ManageDatabaseV2" {...REDACT_EVERYTHING}>
      <div className="w-full overflow-y-auto bg-gray-50">
        <div className="px-md pt-md mb-xs">
          <BreadCrumbs dataSourceName={dataSourceName} />
          <SourceName dataSourceName={dataSourceName} />
        </div>
        <div className="px-md group relative gap-2 flex-col flex">
          {/* {(source?.kind === 'postgres' || source?.kind === 'mssql') && (
            <CollapsibleResource
              title="Template Gallery"
              tooltip="Apply pre-created sets of SQL migrations and Hasura metadata."
            >
              <TemplateGallery driver={source?.kind} showHeader={false} />
            </CollapsibleResource>
          )} */}

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
          <CollapsibleResource
            title="Foreign Key Relationships"
            tooltip="Track foreign key relationships in your database in your GraphQL API"
          >
            <ManageTrackedRelationshipsContainer
              dataSourceName={dataSourceName}
            />
          </CollapsibleResource>
        </div>
      </div>
    </Analytics>
  );
};
