import get from 'lodash/get';
import React from 'react';
import { FaCode, FaDatabase, FaLink, FaTable } from 'react-icons/fa';
import TemplateGallery from '../../../components/Services/Data/Schema/TemplateGallery/TemplateGallery';
import { Tabs as TabUI } from '../../../new-components/Tabs';
import { Analytics, REDACT_EVERYTHING } from '../../Analytics';
import { MetadataSelectors, useMetadata } from '../../hasura-metadata-api';
import { ManageTrackedTables } from '../ManageTable/components/ManageTrackedTables';
import { ManageTrackedFunctions } from '../TrackResources/TrackFunctions/components/ManageTrackedFunctions';
import { ManageSuggestedRelationships } from '../TrackResources/TrackRelationships/ManageSuggestedRelationships';
import { useDriverCapabilities } from '../hooks/useDriverCapabilities';
import { TAB_COLORS } from './constants';
import { BreadCrumbs, CollapsibleResource, SourceName } from './parts';

export interface ManageDatabaseProps {
  dataSourceName: string;
  schema?: string;
}

// hard coding this instead of feature flag until we are sure the new tab UI is accepted.
const USE_TABS = true;

//This component has the code for template gallery but is currently commented out until further notice.
export const ManageDatabase = ({
  dataSourceName,
  schema,
}: ManageDatabaseProps) => {
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
          <SourceName dataSourceName={dataSourceName} schema={schema} />
        </div>
        <div className="px-md group relative gap-2 flex-col flex">
          {USE_TABS ? (
            <Tabs
              dataSourceName={dataSourceName}
              areForeignKeysSupported={areForeignKeysSupported}
              areUserDefinedFunctionsSupported={
                areUserDefinedFunctionsSupported
              }
              schema={schema}
            />
          ) : (
            <Collapsibles
              dataSourceName={dataSourceName}
              areForeignKeysSupported={areForeignKeysSupported}
              areUserDefinedFunctionsSupported={
                areUserDefinedFunctionsSupported
              }
            />
          )}
        </div>
      </div>
    </Analytics>
  );
};

type ContentProps = ManageDatabaseProps & {
  areUserDefinedFunctionsSupported: boolean;
  areForeignKeysSupported: boolean;
};

const Tabs = ({
  dataSourceName,
  areForeignKeysSupported,
  areUserDefinedFunctionsSupported,
  schema,
}: ContentProps) => {
  const { data: source } = useMetadata(
    MetadataSelectors.findSource(dataSourceName)
  );
  const [currentTab, setCurrentTab] = React.useState('tables');

  const tabItems = React.useMemo(
    () => [
      {
        content: (
          <ManageTrackedTables
            dataSourceName={dataSourceName}
            key={dataSourceName}
          />
        ),
        label: source?.kind === 'mongo' ? 'Collections' : 'Tables/Views',
        value: 'tables',
        icon: <FaTable />,
      },
      ...(areForeignKeysSupported
        ? [
            {
              content: (
                <ManageSuggestedRelationships dataSourceName={dataSourceName} />
              ),
              label: 'Foreign Key Relationships',
              value: 'relationships',
              icon: <FaLink />,
            },
          ]
        : []),
      ...(areUserDefinedFunctionsSupported
        ? [
            {
              content: (
                <ManageTrackedFunctions dataSourceName={dataSourceName} />
              ),
              label: 'Functions',
              value: 'functions',
              icon: <FaCode />,
            },
          ]
        : []),
      ...(source?.kind === 'postgres' && !schema
        ? [
            {
              content: (
                <div className="mt-4">
                  <TemplateGallery showHeader={false} driver="postgres" />
                </div>
              ),
              label: 'Template Gallery',
              value: 'template_gallery',
              icon: <FaDatabase />,
            },
          ]
        : []),
    ],
    [
      areForeignKeysSupported,
      areUserDefinedFunctionsSupported,
      dataSourceName,
      schema,
      source?.kind,
    ]
  );
  return (
    <TabUI
      color={TAB_COLORS.primary}
      accentStyle="background"
      value={currentTab}
      onValueChange={setCurrentTab}
      items={tabItems}
    />
  );
};

// leaving this in for now just in case we get any push back on the new tab UI
const Collapsibles = ({
  dataSourceName,
  areUserDefinedFunctionsSupported,
  areForeignKeysSupported,
}: ContentProps) => {
  const { data: source } = useMetadata(
    MetadataSelectors.findSource(dataSourceName)
  );

  const isMongoDB = source?.kind === 'mongo';
  const trackTablesTitle = isMongoDB ? 'Collections' : 'Tables/Views';
  const trackTablesTooltip = `Expose the ${
    isMongoDB ? 'collections' : 'tables'
  } available in your database via the GraphQL API`;

  return (
    <>
      <CollapsibleResource
        title={trackTablesTitle}
        tooltip={trackTablesTooltip}
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
          <ManageSuggestedRelationships dataSourceName={dataSourceName} />
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
    </>
  );
};
