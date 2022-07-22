import { Collapse } from '@/new-components/Collapse';
import React, { useState } from 'react';
import { UseFormRegisterReturn, UseFormReturn } from 'react-hook-form';
import { FaExclamationCircle } from 'react-icons/fa';
import { getTrackingTableFormPlaceholders } from './utils';

type InputFieldProps = {
  name: string;
  placeholder: string;
  formMethods: UseFormRegisterReturn;
  onChange?: (value: string) => void;
};

// TODO NEXT: replace with InputField component when the horizontal version is ready
const InputField: React.FC<InputFieldProps> = ({
  name,
  placeholder,
  formMethods,
  onChange,
}) => {
  return (
    <div className="grid grid-cols-12 gap-3">
      <div className="col-span-4 flex items-center">
        <label className="block font-normal">{name}</label>
      </div>
      <div className="col-span-8">
        <input
          type="text"
          className="block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus:outline-0 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
          placeholder={placeholder}
          {...formMethods}
          onChange={e => {
            formMethods.onChange(e);
            if (onChange) {
              onChange(e.target.value);
            }
          }}
        />
      </div>
    </div>
  );
};

export type FormValues = {
  custom_name: string;
  select: string;
  select_by_pk: string;
  select_aggregate: string;
  insert: string;
  insert_one: string;
  update: string;
  update_by_pk: string;
  delete: string;
  delete_by_pk: string;
};

type TableTrackingCustomizationFormProps = {
  initialTableName: string;
  formMethods: UseFormReturn<FormValues>;
};

export const TableTrackingCustomizationForm: React.FC<TableTrackingCustomizationFormProps> = ({
  initialTableName,
  formMethods,
}) => {
  const [customTableName, setCustomTableName] = useState('');
  const placeholders = getTrackingTableFormPlaceholders(
    customTableName || initialTableName
  );
  const {
    formState: { errors },
  } = formMethods;

  const customNameFormMethods = formMethods.register('custom_name', {
    required: true,
  });
  const selectFormMethods = formMethods.register('select');
  const selectByPkFormMethods = formMethods.register('select_by_pk');
  const selectAggregateFormMethods = formMethods.register('select_aggregate');
  const insertFormMethods = formMethods.register('insert');
  const insertOneFormMethods = formMethods.register('insert_one');
  const updateFormMethods = formMethods.register('update');
  const updateByPkFormMethods = formMethods.register('update_by_pk');
  const deleteFormMethods = formMethods.register('delete');
  const deleteByPkFormMethods = formMethods.register('delete_by_pk');

  const queryAndSubscriptionFields = [
    {
      name: 'Select',
      placeholder: placeholders.select,
      formMethods: selectFormMethods,
    },
    {
      name: 'Select by PK',
      placeholder: placeholders.select_by_pk,
      formMethods: selectByPkFormMethods,
    },
    {
      name: 'Select Aggregate',
      placeholder: placeholders.select_aggregate,
      formMethods: selectAggregateFormMethods,
    },
  ];

  const mutationFields = [
    {
      name: 'Insert',
      placeholder: placeholders.insert,
      formMethods: insertFormMethods,
    },
    {
      name: 'Insert One',
      placeholder: placeholders.insert_one,
      formMethods: insertOneFormMethods,
    },
    {
      name: 'Update',
      placeholder: placeholders.update,
      formMethods: updateFormMethods,
    },
    {
      name: 'Update by PK',
      placeholder: placeholders.update_by_pk,
      formMethods: updateByPkFormMethods,
    },
    {
      name: 'Delete',
      placeholder: placeholders.delete,
      formMethods: deleteFormMethods,
    },
    {
      name: 'Delete by PK',
      placeholder: placeholders.delete_by_pk,
      formMethods: deleteByPkFormMethods,
    },
  ];

  return (
    <>
      <div className="px-sm pb-sm pt-1">
        <div className="pl-6">
          <InputField
            name="Custom Table Name"
            placeholder={placeholders.custom_name}
            formMethods={customNameFormMethods}
            onChange={value => {
              setCustomTableName(value);
            }}
          />
        </div>
        {errors.custom_name?.type === 'required' && (
          <div className="grid grid-cols-12 gap-3">
            <div className="col-span-4 flex items-center" />
            <div className="col-span-8">
              <div
                role="alert"
                aria-label="custom table name is a required field!"
                className="text-red-600 flex items-center text-sm pt-1"
              >
                <span className="flex items-center">
                  <FaExclamationCircle className="mr-1" />
                  This field is required!
                </span>
              </div>
            </div>
          </div>
        )}

        <div className="mb-sm">
          <div className="flex items-center">
            <Collapse title="Query and Subscription" rootClassName="w-full">
              <Collapse.Content>
                <div className="pl-sm py-xs ml-[0.47rem]">
                  <div className="space-y-sm">
                    {queryAndSubscriptionFields.map(field => (
                      <InputField
                        key={`query-and-subscription-${field.name}`}
                        name={field.name}
                        placeholder={field.placeholder}
                        formMethods={field.formMethods}
                      />
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
                    {mutationFields.map(field => (
                      <InputField
                        key={`mutation-${field.name}`}
                        name={field.name}
                        placeholder={field.placeholder}
                        formMethods={field.formMethods}
                      />
                    ))}
                  </div>
                </div>
              </Collapse.Content>
            </Collapse>
          </div>
        </div>
      </div>
    </>
  );
};
