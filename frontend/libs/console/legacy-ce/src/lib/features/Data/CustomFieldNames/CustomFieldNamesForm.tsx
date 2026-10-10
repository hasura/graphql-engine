import { Analytics } from '@hasura/shared/analytics';
import { MetadataTableConfig } from '@hasura/shared/types';
import { Flex } from '@radix-ui/themes';
import {
  Button,
  Collapsible,
  CollapsibleHeader,
  GraphQLSanitizedInputField,
  IndicatorCard,
  SanitizeTips,
  SelectField,
  DialogFooter,
} from '@hasura/shared/ui';
import React from 'react';
import { useCustomFieldNamesForm } from './hooks';
import { CustomFieldNamesFormVals } from './types';
import { mutation_field_props, query_field_props } from './utils';
import { useMetadata } from '@hasura/metadata/api';
import { MetadataSelectors } from '@hasura/metadata/helpers';
import {
  useDriverCapabilities,
  supportsSchemaLessTables,
} from '@hasura/metadata/data-source';

export type CustomFieldNamesFormProps = {
  initialTableName: string;
  currentConfiguration?: MetadataTableConfig;
  onSubmit: (
    data: CustomFieldNamesFormVals,
    configuration: MetadataTableConfig,
  ) => void;
  onClose: () => void;
  callToAction?: string;
  callToActionLoadingText?: string;
  callToDeny?: string;
  isLoading: boolean;
  source: string;
};

export const CustomFieldNamesForm: React.FC<CustomFieldNamesFormProps> = (
  props,
) => {
  const {
    isLoading,
    onClose,
    callToAction = 'Save',
    callToActionLoadingText = 'Saving...',
    callToDeny = 'Cancel',
  } = props;

  const {
    errors,
    Form,
    handleSubmit,
    hasValues,
    isMutateOpen,
    isQueryOpen,
    placeholders,
    reset,
  } = useCustomFieldNamesForm(props);
  const { data: source } = useMetadata(
    MetadataSelectors.findSource(props.source),
  );
  const { data: capabilities } = useDriverCapabilities({
    source,
  });
  const logicalModels = source?.logical_models;

  return (
    <Form onSubmit={handleSubmit}>
      <div>
        <div>
          <SanitizeTips />
          <Flex justify="end" className="mb-4">
            <Button disabled={!hasValues} size="sm" onClick={reset}>
              Clear All Fields
            </Button>
          </Flex>

          <div>
            <Analytics name="custom_name" htmlAttributesToRedact="value">
              <GraphQLSanitizedInputField
                hideTips
                name="custom_name"
                label="Custom Table Name"
                fieldProps={{
                  clearable: true,
                  placeholder: placeholders.custom_name,
                }}
              />
            </Analytics>
            {supportsSchemaLessTables(capabilities) && (
              <Analytics name="logical_model" htmlAttributesToRedact="value">
                <SelectField
                  name="logical_model"
                  placeholder={placeholders.logical_model}
                  options={
                    logicalModels?.map((model) => ({
                      label: model.name,
                      value: model.name,
                    })) ?? []
                  }
                />
              </Analytics>
            )}
          </div>
          {errors.custom_name?.type === 'required' && (
            <div className="grid grid-cols-12 gap-3">
              <div />
              <div className="col-span-8">
                <IndicatorCard
                  showIcon
                  role="alert"
                  aria-label="custom table name is a required field!"
                >
                  This field is required!
                </IndicatorCard>
              </div>
            </div>
          )}

          <div className="mb-2">
            <Flex align="center">
              <Collapsible
                defaultOpen={isQueryOpen}
                className="w-full"
                triggerChildren={
                  <CollapsibleHeader title="Query and Subscription" />
                }
              >
                <div className="space-y-2 w-full">
                  {query_field_props.map((name) => (
                    <Analytics
                      key={`query-and-subscription-${name}`}
                      name={name}
                      htmlAttributesToRedact="value"
                    >
                      <GraphQLSanitizedInputField
                        hideTips
                        name={name}
                        label={name}
                        fieldProps={{
                          clearable: true,
                          placeholder: placeholders[name],
                        }}
                      />
                    </Analytics>
                  ))}
                </div>
              </Collapsible>
            </Flex>
          </div>

          <div>
            <Flex align="center">
              <Collapsible
                defaultOpen={isMutateOpen}
                className="w-full"
                triggerChildren={<CollapsibleHeader title="Mutation" />}
              >
                <div className="space-y-2">
                  {mutation_field_props.map((name) => (
                    <Analytics
                      key={`mutation-${name}`}
                      name={name}
                      htmlAttributesToRedact="value"
                    >
                      <GraphQLSanitizedInputField
                        hideTips
                        label={name}
                        name={name}
                        fieldProps={{
                          clearable: true,
                          placeholder: placeholders[name],
                        }}
                      />
                    </Analytics>
                  ))}
                </div>
              </Collapsible>
            </Flex>
          </div>
        </div>
        {/*
              Implementing a custom footer here because there's no way to submit the form from the footer buttons otherwise.
              Since this creates buttons within the form, the form submit is automatically triggered when these buttons are clicked.
              The only other approach would be to wrap the form around the entire dialog.
              However, if this is done, the form stays rendered in memory when the dialog is opened and closed and must be manually reset and reinit'd each time it happens
              This ended up being the simplest approach.
            */}
        <DialogFooter
          callToAction={callToAction}
          isLoading={isLoading}
          callToActionProps={{
            loadingText: callToActionLoadingText,
          }}
          callToDeny={callToDeny}
          onClose={onClose}
          className="sticky w-full bottom-0 left-0"
        />
      </div>
    </Form>
  );
};
