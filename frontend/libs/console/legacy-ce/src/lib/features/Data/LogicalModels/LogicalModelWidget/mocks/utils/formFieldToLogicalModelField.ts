import { LogicalModel } from '../../../../../hasura-metadata-types';
import { AddLogicalModelFormData } from '../../validationSchema';

export function formFieldToLogicalModelField(
  field: AddLogicalModelFormData['fields'][number]
): LogicalModel['fields'][number] {
  let type: LogicalModel['fields'][number]['type'];
  if (field.array) {
    if (field.typeClass === 'scalar') {
      type = {
        array: {
          scalar: field.type,
          nullable: field.nullable,
        },
      };
    } else {
      type = {
        array: {
          logical_model: field.type,
          nullable: field.nullable,
        },
      };
    }
  } else {
    if (field.typeClass === 'scalar') {
      type = {
        scalar: field.type,
        nullable: field.nullable,
      };
    } else {
      type = {
        logical_model: field.type,
        nullable: field.nullable,
      };
    }
  }
  return {
    name: field.name,
    type,
  };
}
