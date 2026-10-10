import { FlattenCustomType } from '../../../../shared/utils/hasuraCustomTypeUtils';
import { CustomTypeObjectRelationship } from '@hasura/shared/types';
import type { CustomTypeObjectRelationshipFormState } from '../../types';
import { defaultRelFieldMapping } from '../Form/state';

export const getDefaultCustomTypeRelationship =
  (): CustomTypeObjectRelationshipFormState => {
    return {
      name: '',
      type: 'array',
      source: '',
      remote_table: undefined,
      field_mapping: [defaultRelFieldMapping],
    };
  };

export const parseCustomTypeRelationship = (
  relConfig: CustomTypeObjectRelationship,
): CustomTypeObjectRelationshipFormState => {
  const localRelConfig: CustomTypeObjectRelationshipFormState = {
    name: relConfig.name,
    type: relConfig.type,
    source: relConfig.source,
    remote_table: relConfig.remote_table,
    field_mapping: Object.entries(relConfig.field_mapping).map(
      ([field, column]) => {
        return {
          field,
          column,
        };
      },
    ),
  };

  localRelConfig.field_mapping.push(defaultRelFieldMapping);

  return localRelConfig;
};

export const getRelValidationError = (
  relConfig: CustomTypeObjectRelationshipFormState,
) => {
  if (!relConfig.name) return 'relationship name is mandatory';
  if (!relConfig.type) {
    return 'relationship type is mandatory; choose "array" or "object"';
  }
  if (!relConfig.remote_table) return 'please select a reference table';
  if (!relConfig.source) return 'please select a reference data source';
  if (relConfig.field_mapping.length < 2) {
    return 'please choose the mapping between table column(s) and type field(s)';
  }
  return null;
};

export const getScalarOutputType = (typeName: string): FlattenCustomType => ({
  kind: 'scalars',
  definition: {
    name: typeName,
  },
});
