import { Collapsible } from '../../../../new-components/Collapsible';
import { InputField } from '../../../../new-components/Form';
import React from 'react';
import { SectionHeader } from './parts';

export const CustomizationForm: React.VFC<{ defaultOpen?: boolean }> = ({
  defaultOpen,
}) => {
  return (
    <div className="max-w-xl">
      <Collapsible
        defaultOpen={defaultOpen === true}
        triggerChildren={
          <span className="font-semibold capitalize">
            GraphQL Field Customization
          </span>
        }
      >
        <SectionHeader
          header="RootFields"
          tip="Set a namespace or add a prefix / suffix to the root fields for the database's objects in the GraphQL API"
        />
        <div className="ml-4">
          <InputField
            name="customization.root_fields.namespace"
            label="Namespace"
            prependLabel=""
          />
          <InputField name="customization.root_fields.prefix" label="Prefix" />
          <InputField name="customization.root_fields.suffix" label="Suffix" />
        </div>
        <SectionHeader
          header="Type Names"
          tip="Add a prefix / suffix to the types for the database's objects in the GraphQL API"
        />
        <div className="ml-4">
          <InputField name="customization.type_names.prefix" label="Prefix" />
          <InputField name="customization.type_names.suffix" label="Suffix" />
        </div>
      </Collapsible>
    </div>
  );
};
