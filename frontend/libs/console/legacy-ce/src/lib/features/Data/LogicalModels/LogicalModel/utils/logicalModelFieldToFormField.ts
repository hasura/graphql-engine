import { LogicalModel } from '../../../../hasura-metadata-types';
import {
  isArrayLogicalModelType,
  isArrayScalarFieldType,
  isLogicalModelType,
  isScalarFieldType,
} from '../../../../hasura-metadata-types/source/typeGuards';
import { AddLogicalModelFormData } from '../../LogicalModelWidget/validationSchema';

export function logicalModelFieldToFormField(
  f: LogicalModel['fields'][number]
): AddLogicalModelFormData['fields'][number] {
  if (isScalarFieldType(f.type)) {
    return {
      name: f.name,
      type: f.type.scalar,
      typeClass: 'scalar',
      nullable: f.type.nullable,
      array: false,
    };
  }
  if (isLogicalModelType(f.type)) {
    return {
      name: f.name,
      type: f.type.logical_model,
      typeClass: 'logical_model',
      nullable: f.type.nullable,
      array: false,
    };
  }
  if (isArrayLogicalModelType(f.type)) {
    return {
      name: f.name,
      type: f.type.array.logical_model,
      typeClass: 'logical_model',
      nullable: f.type.array.nullable,
      array: true,
    };
  }
  if (isArrayScalarFieldType(f.type)) {
    return {
      name: f.name,
      type: f.type.array.scalar,
      typeClass: 'scalar',
      nullable: f.type.array.nullable,
      array: true,
    };
  }
  throw new Error('Unknown field type');
}
