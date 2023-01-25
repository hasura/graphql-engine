import { NamingConventionOptions } from '@/metadata/types';
import { ConnectDBState } from '../state';

export type TypeNamesField = {
  id: 'prefix' | 'suffix';
  label: string;
  placeholder: string;
};

export const TypeNames: TypeNamesField[] = [
  { label: 'Prefix', placeholder: 'prefix_', id: 'prefix' },
  { label: 'Suffix', placeholder: '_suffix', id: 'suffix' },
];

export type RootFieldsField = {
  id: 'prefix' | 'suffix' | 'namespace';
  label: string;
  placeholder: string;
};

export const RootFields: RootFieldsField[] = [
  { label: 'Namespace', placeholder: 'Namespace...', id: 'namespace' },
  ...TypeNames,
];

export type CustomizationFieldName =
  | 'rootFields.namespace'
  | 'rootFields.prefix'
  | 'rootFields.suffix'
  | 'typeNames.prefix'
  | 'typeNames.suffix'
  | 'namingConvention'
  | string;

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
  onChange: (
    fieldName: CustomizationFieldName,
    fieldValue: string | null | undefined
  ) => void;
  connectionDBState?: ConnectDBState;
  namingConvention?: NamingConventionOptions | null;
};
