import { Collapse } from '@/new-components/Collapse';
import React from 'react';

type FormRowProps = {
  name: string;
  label: string;
  placeholder: string;
  value?: string;
  onChange: (value: string) => void;
};

const FormRow: React.FC<FormRowProps> = ({
  name,
  label,
  placeholder,
  value = '',
  onChange,
}) => (
  <>
    <label
      htmlFor={name}
      className="font-normal px-sm py-xs text-gray-600 w-1/3"
    >
      {label}
    </label>
    <span className="px-sm py-xs">
      <input
        type="text"
        name={name}
        aria-label={name}
        placeholder={placeholder}
        className="form-control font-normal"
        defaultValue={value}
        onChange={e => onChange(e.target.value)}
      />
    </span>
  </>
);

type TypeNamesField = {
  id: 'prefix' | 'suffix';
  label: string;
  placeholder: string;
};

const TypeNames: TypeNamesField[] = [
  { label: 'Prefix', placeholder: 'prefix_', id: 'prefix' },
  { label: 'Suffix', placeholder: '_suffix', id: 'suffix' },
];

type RootFieldsField = Omit<TypeNamesField, 'id'> & {
  id: TypeNamesField['id'] | 'namespace';
};

const RootFields: RootFieldsField[] = [
  { label: 'Namespace', placeholder: 'Namespace...', id: 'namespace' },
  ...TypeNames,
];

export type CustomizationFieldName =
  | 'rootFields.namespace'
  | 'rootFields.prefix'
  | 'rootFields.suffix'
  | 'typeNames.prefix'
  | 'typeNames.suffix';

export type GraphQLFieldCustomizationProps = {
  rootFields?: {
    namespace?: string;
    prefix?: string;
    suffix?: string;
  };
  typeNames?: {
    prefix?: string;
    suffix?: string;
  };
  onChange: (fieldName: CustomizationFieldName, fieldValue: string) => void;
};

export const GraphQLFieldCustomization: React.FC<GraphQLFieldCustomizationProps> = ({
  rootFields,
  typeNames,
  onChange,
}) => {
  return (
    <div>
      <div className="w-full mb-md">
        <div>
          <div className="cursor-pointer w-full flex-initial align-middle">
            <div className="font-semibold">
              <Collapse
                title="GraphQL Field Customization"
                defaultOpen={false}
                tooltip="Set a namespace or add a prefix / suffix to the root fields and types for the database's objects in the GraphQL API"
              >
                <Collapse.Content>
                  <div>
                    <div className="p-sm text-gray-600 font-semibold">
                      Root Fields
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
                          onChange={(value: string) =>
                            onChange(inputName, value)
                          }
                        />
                      );
                    })}
                  </form>
                  <div>
                    <div className="p-sm text-gray-600 font-semibold">
                      Type Names
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
                          onChange={(value: string) =>
                            onChange(inputName, value)
                          }
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
    </div>
  );
};
