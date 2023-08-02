import {
  ArrayLogicalModelType,
  LogicalModelField,
  LogicalModelType,
  ScalarFieldType,
} from './logicalModel';
import {
  LocalTableArrayRelationship,
  LocalTableObjectRelationship,
  SameTableObjectRelationship,
} from './relationships';
import { LocalArrayRelationship, LocalObjectRelationship } from './table';

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

export const isArrayFkRelationship = (
  rel: LocalArrayRelationship
): rel is LocalTableArrayRelationship | LocalTableObjectRelationship => {
  return !('manual_configuration' in rel.using);
};

export const isObjectFkRelationship = (
  rel: LocalObjectRelationship
): rel is LocalTableObjectRelationship | SameTableObjectRelationship => {
  return !('manual_configuration' in rel.using);
};
