import React, { useState } from 'react';
import { NamingConventionOptions } from '@/metadata/types';
import { IconTooltip } from '@/new-components/Tooltip';
import { useServerConfig } from '@/hooks';
import { getSupportedDrivers } from '@/dataSources';
import { CardRadioGroup } from '@/new-components/CardRadioGroup';
import { GraphQLFieldCustomizationProps } from './types';

const namingConventionRadioGroupItems: {
  value: NamingConventionOptions;
  title: string;
  body: React.ReactNode;
}[] = [
  {
    value: 'hasura-default',
    title: 'hasura-default',
    body: (
      <div className="font-thin">
        <li>All names will use snake_case</li>
        <li> Enum values will not be changed</li>
      </div>
    ),
  },
  {
    value: 'graphql-default',
    title: 'graphql-default',
    body: (
      <div className="font-thin">
        <li>
          Field names, argument names, and <br />{' '}
          <p className="pl-6 pb-0">boolean operators will be camelCased</p>
        </li>
        <li>Type names will be PascalCased</li>
        <li>Enum values will be UPPERCASED</li>
      </div>
    ),
  },
];

export const NamingConvention: React.FC<GraphQLFieldCustomizationProps> = ({
  onChange,
  namingConvention,
  connectionDBState,
}) => {
  const { data: configData, isLoading, isError } = useServerConfig();
  const [enableNamingConvention, setEnableNamingConvention] = useState(
    !!namingConvention
  );

  const namingConventionEnabled = () => {
    if (enableNamingConvention) {
      onChange('namingConvention', null);
    } else {
      onChange(
        'namingConvention',
        configData?.default_naming_convention === null
          ? 'hasura-default'
          : configData?.default_naming_convention
      );
    }

    return setEnableNamingConvention(!enableNamingConvention);
  };

  const isNamingConventionEnabled =
    configData?.experimental_features.includes('naming_convention');

  const isNamingConventionSupported =
    connectionDBState?.dbType &&
    getSupportedDrivers('connectDbForm.namingConvention').includes(
      connectionDBState?.dbType
    );

  const namingconventionFields = () => {
    if (isLoading) {
      return (
        <div className="font-normal flex items-center text-gray-600">
          Loading Naming convention...
        </div>
      );
    }
    if (isError) {
      return (
        <div className="font-normal flex items-center text-gray-600">
          Error in fetching server configuration
        </div>
      );
    }
    return (
      <>
        {!isNamingConventionEnabled ? (
          <div className="font-thin">
            Naming convention is not enabled. To enable naming convention, start
            the Hasura server with environment variable
            <code>
              HASURA_GRAPHQL_EXPERIMENTAL_FEATURES:
              &quot;naming_convention&quot;
            </code>
          </div>
        ) : (
          <>
            <div className="mr-md text-gray-600 checkbox">
              <label className="cursor-pointer flex">
                <input
                  type="checkbox"
                  checked={enableNamingConvention}
                  onChange={() => namingConventionEnabled()}
                  className="cursor-pointer"
                />
                <span className="ml-xs">Enable Naming Convention</span>
              </label>
            </div>
            <span className="p-sm py-8">
              <CardRadioGroup
                items={namingConventionRadioGroupItems}
                onChange={ncType => onChange('namingConvention', ncType)}
                value={namingConvention ?? undefined}
                disabled={!enableNamingConvention}
              />
            </span>
          </>
        )}
      </>
    );
  };

  return (
    <div>
      {isNamingConventionSupported ? (
        <div className="p-sm box-border">
          <p className="flex items-center text-gray-600 font-semibold">
            Naming Convention
            <IconTooltip message="Choose a default naming convention for your auto-generated GraphQL schema objects (fields, types, arguments, etc.)" />
            <a
              href="https://hasura.io/docs/latest/graphql/core/databases/postgres/schema/naming-convention/#set-naming-convention-for-a-particular-source"
              target="_blank"
              rel="noopener noreferrer"
            >
              <span className="italic font-thin text-sm	pl-1">(Know More)</span>
            </a>
          </p>
          {namingconventionFields()}
        </div>
      ) : null}
    </div>
  );
};
