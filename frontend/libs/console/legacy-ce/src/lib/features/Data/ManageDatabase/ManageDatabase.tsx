import React from 'react';
import { Flex } from '@radix-ui/themes';
import { FaCode, FaDatabase, FaLink, FaTable } from 'react-icons/fa';
import TemplateGallery from '../../TemplateGallery/TemplateGallery';
import {
  Tabs as TabUI,
  Button,
  Breadcrumbs,
  RelativeLink,
} from '@hasura/shared/ui';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { ManageTrackedTables } from '../ManageTable/components/ManageTrackedTables';
import { ManageTrackedFunctions } from '../TrackResources/TrackFunctions/components/ManageTrackedFunctions';
import { ManageSuggestedRelationships } from '../TrackResources/TrackRelationships/ManageSuggestedRelationships';
import { SourceName } from './parts';
import { SetURLSearchParams, useSearchParams } from 'react-router';
import {
  getDatabaseMethods,
  useDriverCapabilities,
} from '@hasura/metadata/data-source';
import { Source } from '@hasura/shared/types';
import { useDataSourceContext } from '../context/DataSourceContext';
import { dataRoutes } from '@hasura/shared/utils';
import { FaFolder } from 'react-icons/fa6';
import ManageSchema from '../ManageSchema/ManageSchema';

export const ManageDatabase = () => {
  const { currentSource } = useDataSourceContext();
  const [searchParams, setSearchParams] = useSearchParams();
  const schema = searchParams.get('schema');

  const {
    data: {
      areForeignKeysSupported = false,
      areUserDefinedFunctionsSupported = false,
    } = {},
  } = useDriverCapabilities(
    {
      source: currentSource,
    },
    {
      select: (data) => {
        return {
          areForeignKeysSupported: Boolean(
            data.data_schema?.supports_foreign_keys,
          ),
          areUserDefinedFunctionsSupported: Boolean(
            data.user_defined_functions,
          ),
        };
      },
    },
  );

  return (
    <Analytics name="ManageDatabaseV2" {...REDACT_EVERYTHING}>
      <div className="p-6 w-full overflow-y-auto">
        <div>
          <Breadcrumbs
            className="mb-4"
            items={[
              {
                title: 'Data',
                url: dataRoutes.manageDatabase,
              },
              {
                title: currentSource.name,
                icon: <FaDatabase />,
              },
            ]}
          />
          <Flex align="center" gap="2">
            <SourceName source={currentSource} />
            <RelativeLink to={dataRoutes.permissionSummary(currentSource.name)}>
              <Button mode="default" size="sm">
                Show Permissions Summary
              </Button>
            </RelativeLink>
          </Flex>
        </div>
        <Flex direction="column" gap="2" className="relative">
          <Tabs
            source={currentSource}
            areForeignKeysSupported={areForeignKeysSupported}
            areUserDefinedFunctionsSupported={areUserDefinedFunctionsSupported}
            schema={schema}
            searchParams={searchParams}
            setSearchParams={setSearchParams}
          />
        </Flex>
      </div>
    </Analytics>
  );
};

type ContentProps = {
  source: Source;
  searchParams: URLSearchParams;
  setSearchParams: SetURLSearchParams;
  schema?: string | null;
  areUserDefinedFunctionsSupported: boolean;
  areForeignKeysSupported: boolean;
};

const Tabs = ({
  source,
  searchParams,
  setSearchParams,
  areForeignKeysSupported,
  areUserDefinedFunctionsSupported,
  schema,
}: ContentProps) => {
  const dbMethods = getDatabaseMethods(source.kind);

  const tabItems = React.useMemo(
    () => [
      ...(dbMethods.introspection.getDatabaseSchemas
        ? [
            {
              content: (
                <div className="mt-4">
                  <ManageSchema source={source} />
                </div>
              ),
              label: 'Schemas',
              value: 'schemas',
              icon: <FaFolder />,
            },
          ]
        : []),
      {
        content: (
          <div className="mt-4">
            <ManageTrackedTables source={source} key={source.name} />
          </div>
        ),
        label: source?.kind === 'mongodb' ? 'Collections' : 'Tables/Views',
        value: 'tables',
        icon: <FaTable />,
      },
      ...(areForeignKeysSupported
        ? [
            {
              content: <ManageSuggestedRelationships source={source} />,
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
                <div className="mt-4">
                  <ManageTrackedFunctions dataSourceName={source.name} />
                </div>
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
                  <TemplateGallery showHeader={false} source={source} />
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
      source,
      schema,
      source?.kind,
    ],
  );
  return (
    <TabUI
      color="indigo"
      value={searchParams.get('tab') || 'tables'}
      onValueChange={(value) => {
        setSearchParams((prev) => {
          prev.set('tab', value);
          return prev;
        });
      }}
      items={tabItems}
    />
  );
};
