import React, { Dispatch } from 'react';
import { ConnectDBActions } from '../state';
import { GraphQLFieldCustomization } from './GraphQLFieldCustomization';
import {
  CustomizationFieldName,
  GraphQLFieldCustomizationProps,
} from './types';

type GraphQLFieldCustomizationContainerProps = Omit<
  GraphQLFieldCustomizationProps,
  'onChange'
> & {
  connectionDBStateDispatch: Dispatch<ConnectDBActions>;
};

const CustomizationFieldNameToActionTypeMap: Record<
  CustomizationFieldName,
  ConnectDBActions['type']
> = {
  'rootFields.namespace': 'UPDATE_CUSTOMIZATION_ROOT_FIELDS_NAMESPACE',
  'rootFields.prefix': 'UPDATE_CUSTOMIZATION_ROOT_FIELDS_PREFIX',
  'rootFields.suffix': 'UPDATE_CUSTOMIZATION_ROOT_FIELDS_SUFFIX',
  'typeNames.prefix': 'UPDATE_CUSTOMIZATION_TYPE_NAMES_PREFIX',
  'typeNames.suffix': 'UPDATE_CUSTOMIZATION_TYPE_NAMES_SUFFIX',
  namingConvention: 'UPDATE_CUSTOMIZATION_NAMING_CONVENTION',
};

export const getActionType = (
  fieldName: CustomizationFieldName
): ConnectDBActions['type'] | null =>
  CustomizationFieldNameToActionTypeMap[fieldName] || null;

export const GraphQLFieldCustomizationContainer: React.FC<GraphQLFieldCustomizationContainerProps> =
  ({
    rootFields,
    typeNames,
    namingConvention,
    connectionDBStateDispatch,
    connectionDBState,
  }) => {
    const onChange = (
      fieldName: CustomizationFieldName,
      fieldValue: string | null | undefined
    ) => {
      const actionType = getActionType(fieldName);
      if (actionType) {
        connectionDBStateDispatch({
          type: actionType,
          data: fieldValue,
        } as ConnectDBActions);
      }
    };
    return (
      <GraphQLFieldCustomization
        rootFields={rootFields}
        typeNames={typeNames}
        namingConvention={namingConvention}
        onChange={onChange}
        connectionDBState={connectionDBState}
      />
    );
  };
