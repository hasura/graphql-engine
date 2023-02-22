import { Collapse } from '../../../../../new-components/deprecated';
import { IconTooltip } from '../../../../../new-components/Tooltip';
import React from 'react';
import { FormRow } from './FormRow';
import { NamingConvention } from './NamingConvention';
import {
  CustomizationFieldName,
  GraphQLFieldCustomizationProps,
  RootFields,
  TypeNames,
} from './types';

export const GraphQLFieldCustomization = ({
  rootFields,
  typeNames,
  onChange,
  namingConvention,
  connectionDBState,
}: GraphQLFieldCustomizationProps) => {
  return (
    <div>
      <div className="mb-md">
        <div className="w-full flex-initial align-middle">
          <div className="font-semibold">
            <Collapse title="GraphQL Field Customization" defaultOpen={false}>
              <Collapse.Content>
                <NamingConvention
                  onChange={onChange}
                  namingConvention={namingConvention}
                  connectionDBState={connectionDBState}
                />
                <div>
                  <div className="flex items-center p-sm text-gray-600 font-semibold">
                    Root Fields
                    <IconTooltip message="Set a namespace or add a prefix / suffix to the root fields for the database's objects in the GraphQL API" />
                  </div>
                </div>
                <form
                  aria-label="rootFields"
                  className={`grid gap-0 grid-cols-2 grid-rows-${RootFields.length}`}
                >
                  {RootFields.map(({ label, placeholder, id }) => {
                    const inputName: CustomizationFieldName = `rootFields.${id}`;
                    return (
                      <FormRow
                        key={id}
                        name={inputName}
                        label={label}
                        placeholder={placeholder}
                        value={rootFields?.[id]}
                        onChange={(value: string) => onChange(inputName, value)}
                      />
                    );
                  })}
                </form>
                <div>
                  <div className="flex items-center p-sm text-gray-600 font-semibold">
                    Type Names
                    <IconTooltip message="Add a prefix / suffix to the types for the database's objects in the GraphQL API" />
                  </div>
                </div>
                <form
                  aria-label="typeNames"
                  className={`grid gap-0 grid-cols-2 grid-rows-${TypeNames.length}`}
                >
                  {TypeNames.map(({ label, placeholder, id }) => {
                    const inputName: CustomizationFieldName = `typeNames.${id}`;
                    return (
                      <FormRow
                        key={id}
                        name={inputName}
                        label={label}
                        placeholder={placeholder}
                        value={typeNames?.[id]}
                        onChange={(value: string) => onChange(inputName, value)}
                      />
                    );
                  })}
                </form>
              </Collapse.Content>
            </Collapse>
          </div>
        </div>
      </div>
    </div>
  );
};
