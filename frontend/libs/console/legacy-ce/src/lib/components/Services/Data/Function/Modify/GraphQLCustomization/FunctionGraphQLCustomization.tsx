import { useState } from 'react';
import omit from 'lodash/omit';
import { RootField } from '../../../../../../features/Data/ModifyTable/components/TableRootFields';
import { isGDCTable } from '../../../../../../features/DataSource/utils';
import {
  MetadataSelectors,
  useMetadata,
} from '../../../../../../features/hasura-metadata-api';
import {
  QualifiedFunction,
  SupportedDrivers,
} from '../../../../../../features/hasura-metadata-types';
import { Button } from '../../../../../../new-components/Button';
import { Dialog } from '../../../../../../new-components/Dialog';
import { LearnMoreLink } from '../../../../../../new-components/LearnMoreLink';
import { hasuraToast } from '../../../../../../new-components/Toasts';
import { IconTooltip } from '../../../../../../new-components/Tooltip';
import { isArray, isObject } from '../../../../../Common/utils/jsUtils';
import {
  CustomFunctionFieldsForm,
  CustomFunctionFieldsFormValues,
} from './CustomFunctionFieldsForm';
import { useSetFunctionCustomization } from './hooks/useSetFunctionCustomization';

export type FunctionGraphQLCustomizationProps = {
  driver: SupportedDrivers;
  dataSourceName: string;
  qualifiedFunction: QualifiedFunction;
};

const getFunctionName = (fn: QualifiedFunction) => {
  if (isObject(fn) && 'name' in fn) {
    return fn.name as string;
  }

  if (isArray(fn)) {
    return fn.join('.');
  }

  return '';
};

export const FunctionGraphQLCustomization = ({
  driver,
  dataSourceName,
  qualifiedFunction,
}: FunctionGraphQLCustomizationProps) => {
  const [showCustomModal, setShowCustomModal] = useState(false);

  const { data: metadataFunction } = useMetadata(
    MetadataSelectors.findFunction(dataSourceName, qualifiedFunction)
  );

  const formDefaultValues: CustomFunctionFieldsFormValues = {
    custom_name: metadataFunction?.configuration?.custom_name ?? '',
    function:
      metadataFunction?.configuration?.custom_root_fields?.function ?? '',
    function_aggregate:
      metadataFunction?.configuration?.custom_root_fields?.function_aggregate ??
      '',
  };

  const isCustomized =
    metadataFunction?.configuration?.custom_name ||
    (metadataFunction?.configuration?.custom_root_fields &&
      Object.keys(metadataFunction?.configuration?.custom_root_fields).length >
        0);

  const functionName = getFunctionName(metadataFunction?.function);

  const { onSetFunctionCustomization, isLoading } = useSetFunctionCustomization(
    {
      onSuccess: () => {
        hasuraToast({
          title: 'Success!',
          message: 'Custom function fields updated successfully',
          type: 'success',
        });
        setShowCustomModal(false);
      },
      onError: error => {
        hasuraToast({
          title: 'Error',
          message:
            error?.message ?? 'Error while updating custom function fields',
          type: 'error',
        });
      },
    }
  );

  const onSaveCustomFields = (data: CustomFunctionFieldsFormValues) => {
    const areCustomRootFieldsDefined = data.function || data.function_aggregate;
    const customRootFields = {
      ...(data.function ? { function: data.function } : {}),
      ...(data.function_aggregate
        ? { function_aggregate: data.function_aggregate }
        : {}),
    };

    const newFieldsConfiguration = {
      ...(data.custom_name ? { custom_name: data.custom_name } : {}),
      ...(areCustomRootFieldsDefined
        ? { custom_root_fields: customRootFields }
        : {}),
    };

    const previousConfiguration = omit(
      metadataFunction?.configuration ? metadataFunction.configuration : {},
      ['custom_name', 'custom_root_fields']
    );

    return onSetFunctionCustomization({
      driver,
      dataSourceName,
      functionName: isGDCTable(qualifiedFunction)
        ? qualifiedFunction
        : functionName,
      configuration: {
        ...previousConfiguration,
        ...newFieldsConfiguration,
      },
    });
  };

  return (
    <div className="w-full sm:w-6/12 mb-md">
      <h4 className="flex items-center text-gray-600 font-semibold mb-formlabel">
        Custom Field Names{' '}
        <IconTooltip message="Customize function root names for GraphQL operations." />
        <span className="ml-xs">
          <LearnMoreLink href="https://hasura.io/docs/latest/graphql/core/schema/custom-functions.html#custom-function-root-fields" />
        </span>
      </h4>

      <div>
        <Button onClick={() => setShowCustomModal(true)} className="mb-2">
          {isCustomized ? 'Edit Custom Field Names' : 'Add Custom Field Names'}
        </Button>
        <div className="p-2">
          {!isCustomized && (
            <div className="text-gray-500 mx-2">
              No custom fields are currently set.
            </div>
          )}
          {metadataFunction?.configuration?.custom_name && (
            <RootField
              property="custom_name"
              value={metadataFunction.configuration.custom_name}
            />
          )}
          {Object.entries(
            metadataFunction?.configuration?.custom_root_fields ?? {}
          ).map(([key, value]) => (
            <RootField key={key} property={key} value={value} />
          ))}
        </div>
      </div>
      {showCustomModal && (
        <Dialog
          hasBackdrop
          title={functionName}
          description={''}
          onClose={() => setShowCustomModal(false)}
          titleTooltip="Customize function name and root fields for GraphQL operations."
        >
          <CustomFunctionFieldsForm
            onSubmit={onSaveCustomFields}
            onClose={() => setShowCustomModal(false)}
            isLoading={isLoading}
            defaultValues={formDefaultValues}
          />
        </Dialog>
      )}
    </div>
  );
};
