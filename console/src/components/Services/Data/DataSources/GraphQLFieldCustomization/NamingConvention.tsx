import React from 'react';
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

  if (isError) {
    return <div>Error in fetching server configuration</div>;
  }

  if (isLoading) {
    return <div>Loading...</div>;
  }

  const isNamingConventionEnabled = configData?.experimental_features.includes(
    'naming_convention'
  );

  const isNamingConventionSupported =
    connectionDBState?.dbType &&
    getSupportedDrivers('connectDbForm.namingConvention').includes(
      connectionDBState?.dbType
    );

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
          {!isNamingConventionEnabled ? (
            <div className="font-thin">
              Naming convention is not enabled. To enable naming convention,
              start the Hasura server with environment variable
              <code>
                HASURA_GRAPHQL_EXPERIMENTAL_FEATURES:
                &quot;naming_convention&quot;
              </code>
            </div>
          ) : (
            <span className="p-sm py-8">
              <CardRadioGroup
                items={namingConventionRadioGroupItems}
                onChange={ncType => onChange('namingConvention', ncType)}
                value={namingConvention}
              />
            </span>
          )}
        </div>
      ) : null}
    </div>
  );
};
