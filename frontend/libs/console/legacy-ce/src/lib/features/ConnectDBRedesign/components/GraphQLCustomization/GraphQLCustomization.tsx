import { useServerConfig } from '../../../../hooks';
import {
  GraphQLSanitizedInputField,
  Select,
} from '../../../../new-components/Form';
import { IconTooltip } from '../../../../new-components/Tooltip';

export const GraphQLCustomization = ({ name }: { name: string }) => {
  const { data: configData } = useServerConfig();
  const isNamingConventionEnabled =
    configData?.experimental_features.includes('naming_convention');
  return (
    <div className="my-2">
      {!isNamingConventionEnabled ? (
        <div className="bg-white border border-hasGray-300 rounded-md shadow-sm overflow-hidden p-sm mb-sm">
          Naming convention is not enabled. To enable this, restart your Hasura
          server with the environment variable{' '}
          <code className="bg-red-100 text-red-800 rounded">
            HASURA_GRAPHQL_EXPERIMENTAL_FEATURES: &quot;naming_convention&quot;
          </code>
        </div>
      ) : (
        <Select
          label="Naming Convention"
          options={[
            { label: 'hasura-default', value: 'hasura-default' },
            { label: 'graphql-default', value: 'graphql-default' },
          ]}
          name={`${name}.namingConvention`}
          tooltip="Choose a default naming convention for your auto-generated GraphQL schema objects (fields, types, arguments, etc.)"
        />
      )}

      <div className="bg-white border border-hasGray-300 rounded-md shadow-sm overflow-hidden">
        <div className="bg-white px-3 py-1.5 font-semibold flex">
          Root Fields{' '}
          <IconTooltip message="Set a namespace or add a prefix / suffix to the root fields for the database's objects in the GraphQL API" />
        </div>
        <div className="px-3 pt-1.5 pb-3 border-t border-hasGray-300">
          <GraphQLSanitizedInputField
            label="Namespace"
            name={`${name}.rootFields.namespace`}
            placeholder="namespace_"
            hideTips
          />
          <GraphQLSanitizedInputField
            label="Prefix"
            name={`${name}.rootFields.prefix`}
            placeholder="prefix_"
            hideTips
          />
          <GraphQLSanitizedInputField
            label="Suffix"
            name={`${name}.rootFields.suffix`}
            placeholder="_suffix"
            hideTips
          />
        </div>
      </div>

      <div className="mt-2 bg-white border border-hasGray-300 rounded-md shadow-sm overflow-hidden">
        <div className="bg-white px-3 py-1.5 font-semibold flex">
          Type Names{' '}
          <IconTooltip message="Add a prefix / suffix to the types for the database's objects in the GraphQL API" />
        </div>
        <div className="px-3 pt-1.5 pb-3 border-t border-hasGray-300">
          <GraphQLSanitizedInputField
            label="Prefix"
            name={`${name}.typeNames.prefix`}
            placeholder="prefix_"
            hideTips
          />
          <GraphQLSanitizedInputField
            label="Suffix"
            name={`${name}.typeNames.suffix`}
            placeholder="_suffix"
            hideTips
          />
        </div>
      </div>
    </div>
  );
};
