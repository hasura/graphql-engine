import { TrackingTableFormValues } from '@/components/Services/Data/Schema/tableTrackCustomization/types';
import { MetadataTableConfig } from '@/features/MetadataAPI';
import { Button } from '@/new-components/Button';
import { Collapse } from '@/new-components/deprecated';
import { Dialog } from '@/new-components/Dialog';
import { SanitizeTips } from '@/utils/sanitizeGraphQLFieldNames';
import React from 'react';
import { FormProvider } from 'react-hook-form';
import { FaExclamationCircle } from 'react-icons/fa';
import { useGqlCustomizationForm } from './hooks';
import { InputField } from './parts';
import { mutation_field_props, query_field_props } from './utils';

export type TableTrackingCustomizationFormProps = {
  initialTableName: string;
  currentConfiguration?: MetadataTableConfig;
  onSubmit: (
    data: TrackingTableFormValues,
    configuration: MetadataTableConfig
  ) => void;
  onClose: () => void;
};

export const TableTrackingCustomizationForm: React.VFC<TableTrackingCustomizationFormProps> =
  props => {
    const { onClose } = props;

    const {
      errors,
      formMethods,
      handleSubmit,
      hasValues,
      isMutateOpen,
      isQueryOpen,
      placeholders,
      reset,
      setCustomTableName,
    } = useGqlCustomizationForm(props);

    return (
      <FormProvider {...formMethods}>
        <form onSubmit={formMethods.handleSubmit(handleSubmit)}>
          <div>
            <div className="px-sm pb-sm">
              <SanitizeTips />
              <div className="mb-4 flex justify-end">
                <Button disabled={!hasValues} size="sm" onClick={reset}>
                  Clear All Fields
                </Button>
              </div>

              <div className="pl-6">
                <InputField
                  label="Custom Table Name"
                  fieldName="custom_name"
                  placeholder={placeholders.custom_name}
                  onClear={() => {
                    setCustomTableName('');
                  }}
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
                  <Collapse
                    defaultOpen={isQueryOpen}
                    title="Query and Subscription"
                    rootClassName="w-full"
                  >
                    <Collapse.Content>
                      <div className="pl-sm py-xs ml-[0.47rem]">
                        <div className="space-y-sm">
                          {query_field_props.map(name => (
                            <InputField
                              key={`query-and-subscription-${name}`}
                              fieldName={name}
                              label={name}
                              placeholder={placeholders[name]}
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
                  <Collapse
                    defaultOpen={isMutateOpen}
                    title="Mutation"
                    rootClassName="w-full"
                  >
                    <Collapse.Content>
                      <div className="pl-sm py-xs ml-[0.47rem]">
                        <div className="space-y-sm">
                          {mutation_field_props.map(name => (
                            <InputField
                              key={`mutation-${name}`}
                              label={name}
                              fieldName={name}
                              placeholder={placeholders[name]}
                            />
                          ))}
                        </div>
                      </div>
                    </Collapse.Content>
                  </Collapse>
                </div>
              </div>
            </div>

            <Dialog.Footer
              callToAction="Save"
              callToActionLoadingText="Saving..."
              callToDeny="Cancel"
              onClose={onClose}
              className="absolute w-full bottom-0"
            />
          </div>
        </form>
      </FormProvider>
    );
  };
