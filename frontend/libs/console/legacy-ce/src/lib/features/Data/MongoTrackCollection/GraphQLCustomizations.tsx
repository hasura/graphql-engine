import { GraphQLSanitizedInputField } from '../../../new-components/Form';
import { Analytics } from '../../Analytics';
import {
  query_field_props,
  mutation_field_props,
  customFieldNamesPlaceholders,
} from '../CustomFieldNames/utils';
import { Collapse } from '../../../new-components/deprecated';

type GraphQLCustomizations = {
  customCollectionName: string;
  collectionName: string;
};

export const CollectionGraphQLCustomizations = ({
  customCollectionName,
  collectionName,
}: GraphQLCustomizations) => {
  const placeholders = customFieldNamesPlaceholders(
    customCollectionName || collectionName
  );

  return (
    <div>
      <div className="px-4 pb-sm">
        <div className="text-muted">
          Customize GraphQL fields based on your needs
        </div>

        <div>
          <Analytics name="custom_name" htmlAttributesToRedact="value">
            <GraphQLSanitizedInputField
              hideTips
              clearButton
              name="custom_name"
              label="Custom Collection Name"
              placeholder={placeholders.custom_name}
            />
          </Analytics>
        </div>

        <div className="mb-sm">
          <div className="flex items-center">
            <Collapse title="Query and Subscription" rootClassName="w-full">
              <Collapse.Content>
                <div className="pl-sm py-xs ml-[0.47rem]">
                  <div className="space-y-sm">
                    {query_field_props.map(name => (
                      <Analytics
                        key={`query-and-subscription-${name}`}
                        name={`custom_root_fields.${name}`}
                        htmlAttributesToRedact="value"
                      >
                        <GraphQLSanitizedInputField
                          clearButton
                          hideTips
                          name={`custom_root_fields.${name}`}
                          label={name}
                          placeholder={placeholders[name]}
                        />
                      </Analytics>
                    ))}
                  </div>
                </div>
              </Collapse.Content>
            </Collapse>
          </div>
        </div>

        <div>
          <div className="flex items-center">
            <Collapse title="Mutation" rootClassName="w-full">
              <Collapse.Content>
                <div className="pl-sm py-xs ml-[0.47rem]">
                  <div className="space-y-sm">
                    {mutation_field_props.map(name => (
                      <Analytics
                        key={`mutation-${name}`}
                        name={name}
                        htmlAttributesToRedact="value"
                      >
                        <GraphQLSanitizedInputField
                          clearButton
                          hideTips
                          name={`custom_root_fields.${name}`}
                          label={name}
                          placeholder={placeholders[name]}
                        />
                      </Analytics>
                    ))}
                  </div>
                </div>
              </Collapse.Content>
            </Collapse>
          </div>
        </div>
      </div>
    </div>
  );
};
