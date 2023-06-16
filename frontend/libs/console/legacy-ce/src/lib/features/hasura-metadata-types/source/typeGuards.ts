import {
  ArrayLogicalModelType,
  LogicalModelField,
  LogicalModelType,
  ScalarFieldType,
} from './logicalModel';

export const isScalarFieldType = (
  fieldType: LogicalModelField['type']
): fieldType is ScalarFieldType => {
  return 'scalar' in fieldType;
};

export const isLogicalModelType = (
  fieldType: LogicalModelField['type']
): fieldType is LogicalModelType => {
  return 'logical_model' in fieldType;
};

export const isArrayLogicalModelType = (
  fieldType: LogicalModelField['type']
): fieldType is ArrayLogicalModelType => {
  return 'array' in fieldType;
};
