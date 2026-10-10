import { Analytics } from '@hasura/shared/analytics';
import {
  query_field_props,
  mutation_field_props,
  customFieldNamesPlaceholders,
} from '../CustomFieldNames/utils';
import {
  Collapsible,
  CollapsibleHeader,
  GraphQLSanitizedInputField,
} from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

type GraphQLCustomizations = {
  customCollectionName: string;
  collectionName: string;
};

export const CollectionGraphQLCustomizations = ({
  customCollectionName,
  collectionName,
}: GraphQLCustomizations) => {
  const placeholders = customFieldNamesPlaceholders(
    customCollectionName || collectionName,
  );

  return (
    <div>
      <div className="px-4 pb-2">
        <div className="text-muted">
          Customize GraphQL fields based on your needs
        </div>

        <div>
          <Analytics name="custom_name" htmlAttributesToRedact="value">
            <GraphQLSanitizedInputField
              hideTips
              name="custom_name"
              label="Custom Collection Name"
              fieldProps={{
                clearable: true,
                placeholder: placeholders.custom_name,
              }}
            />
          </Analytics>
        </div>

        <div className="mb-2">
          <Flex align="center">
            <Collapsible
              triggerClassName="w-full"
              triggerChildren={
                <CollapsibleHeader title="Query and Subscription" />
              }
            >
              <div className="pl-sm py-xs ml-[0.47rem]">
                <div className="space-y-2">
                  {query_field_props.map((name) => (
                    <Analytics
                      key={`query-and-subscription-${name}`}
                      name={`custom_root_fields.${name}`}
                      htmlAttributesToRedact="value"
                    >
                      <GraphQLSanitizedInputField
                        hideTips
                        name={`custom_root_fields.${name}`}
                        label={name}
                        fieldProps={{
                          clearable: true,
                          placeholder: placeholders[name],
                        }}
                      />
                    </Analytics>
                  ))}
                </div>
              </div>
            </Collapsible>
          </Flex>
        </div>

        <div>
          <Flex align="center">
            <Collapsible
              triggerClassName="w-full"
              triggerChildren={<CollapsibleHeader title="Mutation" />}
            >
              <div className="pl-sm py-xs ml-[0.47rem]">
                <div className="space-y-2">
                  {mutation_field_props.map((name) => (
                    <Analytics
                      key={`mutation-${name}`}
                      name={name}
                      htmlAttributesToRedact="value"
                    >
                      <GraphQLSanitizedInputField
                        hideTips
                        name={`custom_root_fields.${name}`}
                        label={name}
                        fieldProps={{
                          clearable: true,
                          placeholder: placeholders[name],
                        }}
                      />
                    </Analytics>
                  ))}
                </div>
              </div>
            </Collapsible>
          </Flex>
        </div>
      </div>
    </div>
  );
};
