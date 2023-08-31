import { Analytics } from '../../Analytics';
import { MetadataTableConfig } from '../../hasura-metadata-types';
import { Button } from '../../../new-components/Button';
import { Collapse } from '../../../new-components/deprecated';
import { Dialog } from '../../../new-components/Dialog';
import {
  GraphQLSanitizedInputField,
  Select,
} from '../../../new-components/Form';
import { SanitizeTips } from '../../../utils/sanitizeGraphQLFieldNames';
import React from 'react';
import { FaExclamationCircle } from 'react-icons/fa';
import { useCustomFieldNamesForm } from './hooks';
import { CustomFieldNamesFormVals } from './types';
import { mutation_field_props, query_field_props } from './utils';
import { MetadataUtils, useMetadata } from '../../hasura-metadata-api';
import { useDriverCapabilities } from '../hooks/useDriverCapabilities';
import { supportsSchemaLessTables } from '../LogicalModels/LogicalModelWidget/utils';

export type CustomFieldNamesFormProps = {
  initialTableName: string;
  currentConfiguration?: MetadataTableConfig;
  onSubmit: (
    data: CustomFieldNamesFormVals,
    configuration: MetadataTableConfig
  ) => void;
  onClose: () => void;
  callToAction?: string;
  callToActionLoadingText?: string;
  callToDeny?: string;
  isLoading: boolean;
  source: string;
};

export const CustomFieldNamesForm: React.VFC<
  CustomFieldNamesFormProps
> = props => {
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
  const { data: capabilities } = useDriverCapabilities({
    dataSourceName: props.source,
  });
  const { data: logicalModels } = useMetadata(
    m => MetadataUtils.findMetadataSource(props.source, m)?.logical_models
  );

  return (
    <Form onSubmit={handleSubmit}>
      <div>
        <div className="px-4 pb-sm">
          <SanitizeTips />
          <div className="mb-4 flex justify-end">
            <Button disabled={!hasValues} size="sm" onClick={reset}>
              Clear All Fields
            </Button>
          </div>

          <div>
            <Analytics name="custom_name" htmlAttributesToRedact="value">
              <GraphQLSanitizedInputField
                hideTips
                clearButton
                name="custom_name"
                label="Custom Table Name"
                placeholder={placeholders.custom_name}
              />
            </Analytics>
            {supportsSchemaLessTables(capabilities) && (
              <Analytics name="logical_model" htmlAttributesToRedact="value">
                <Select
                  name="logical_model"
                  placeholder={placeholders.logical_model}
                  options={
                    logicalModels?.map(model => ({
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
                        <Analytics
                          key={`query-and-subscription-${name}`}
                          name={name}
                          htmlAttributesToRedact="value"
                        >
                          <GraphQLSanitizedInputField
                            clearButton
                            hideTips
                            name={name}
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
              <Collapse
                defaultOpen={isMutateOpen}
                title="Mutation"
                rootClassName="w-full"
              >
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
                            label={name}
                            name={name}
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
        {/*
              Implementing a custom footer here because there's no way to submit the form from the footer buttons otherwise.
              Since this creates buttons within the form, the form submit is automatically triggered when these buttons are clicked.
              The only other approach would be to wrap the form around the entire dialog.
              However, if this is done, the form stays rendered in memory when the dialog is opened and closed and must be manually reset and reinit'd each time it happens
              This ended up being the simplest approach.
            */}
        <Dialog.Footer
          callToAction={callToAction}
          isLoading={isLoading}
          callToActionLoadingText={callToActionLoadingText}
          callToDeny={callToDeny}
          onClose={onClose}
          className="sticky w-full bottom-0 left-0"
        />
      </div>
    </Form>
  );
};
