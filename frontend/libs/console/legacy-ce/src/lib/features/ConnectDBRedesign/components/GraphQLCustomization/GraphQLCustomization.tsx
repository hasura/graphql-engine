import {
  GraphQLSanitizedInputField,
  SelectField,
  IconTooltip,
  Card,
  Text,
  Separator,
} from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

export const GraphQLCustomization = ({ name }: { name: string }) => {
  return (
    <div className="my-2">
      <div className="mt-2">
        <SelectField
          label="Naming Convention"
          placeholder="-- Select --"
          full
          options={[
            { label: 'hasura-default', value: 'hasura-default' },
            { label: 'graphql-default', value: 'graphql-default' },
          ]}
          name={`${name}.namingConvention`}
          tooltip="Choose a default naming convention for your auto-generated GraphQL schema objects (fields, types, arguments, etc.)"
        />
      </div>
      <Card size="2">
        <Flex className="px-3 py-1.5" align="center" gap="2">
          <Text weight="bold">Root Fields</Text>
          <IconTooltip message="Set a namespace or add a prefix / suffix to the root fields for the database's objects in the GraphQL API" />
        </Flex>
        <Separator size="4" />
        <div className="px-3 pt-1.5 pb-3">
          <GraphQLSanitizedInputField
            label="Namespace"
            name={`${name}.rootFields.namespace`}
            fieldProps={{ placeholder: 'namespace_' }}
            hideTips
          />
          <GraphQLSanitizedInputField
            label="Prefix"
            name={`${name}.rootFields.prefix`}
            fieldProps={{ placeholder: 'prefix_' }}
            hideTips
          />
          <GraphQLSanitizedInputField
            label="Suffix"
            name={`${name}.rootFields.suffix`}
            fieldProps={{ placeholder: '_suffix' }}
            hideTips
          />
        </div>
      </Card>

      <Card size="2" className="mt-2">
        <Flex className="px-3 py-1.5" align="center" gap="2">
          <Text weight="bold">Type Names</Text>
          <IconTooltip message="Add a prefix / suffix to the types for the database's objects in the GraphQL API" />
        </Flex>
        <Separator size="4" />
        <div className="px-3 pt-1.5 pb-3">
          <GraphQLSanitizedInputField
            label="Prefix"
            name={`${name}.typeNames.prefix`}
            fieldProps={{ placeholder: 'prefix_' }}
            hideTips
          />
          <GraphQLSanitizedInputField
            label="Suffix"
            name={`${name}.typeNames.suffix`}
            fieldProps={{ placeholder: '_suffix' }}
            hideTips
          />
        </div>
      </Card>
    </div>
  );
};
