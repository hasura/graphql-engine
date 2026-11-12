import {
  Legacy_SourceToRemoteSchemaRelationship,
  ManualArrayRelationship,
  ManualObjectRelationship,
  SourceToRemoteSchemaRelationship,
  SameTableObjectRelationship,
} from '@hasura/shared/types';
import { AllowedTableRelationships } from './types';

function isArrayOfStrings(value: any): value is string[] {
  return (
    Array.isArray(value) && value.every((item) => typeof item === 'string')
  );
}

export const isRemoteSchemaRelationship = (
  relationship: AllowedTableRelationships,
): relationship is SourceToRemoteSchemaRelationship => {
  if (!('definition' in relationship)) return false;

  const definition = relationship.definition;

  if (!('to_remote_schema' in definition)) return false;

  // turn off "obj is declared but never used."
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  const remoteSchemarelationshipDefinition: SourceToRemoteSchemaRelationship['definition'] =
    definition;

  return true;
};

export const isLegacyRemoteSchemaRelationship = (
  relationship: AllowedTableRelationships,
): relationship is Legacy_SourceToRemoteSchemaRelationship => {
  return (
    'definition' in relationship && 'hasura_fields' in relationship.definition
  );
};

export const isManualObjectRelationship = (
  relationship: AllowedTableRelationships,
): relationship is ManualObjectRelationship => {
  if (!('using' in relationship)) return false;

  const constraint = relationship.using;

  if (!('manual_configuration' in constraint)) return false;

  // turn off "obj is declared but never used."
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  const objectRelationshipManualConfig: ManualObjectRelationship['using'] =
    constraint;

  return true;
};

export const isSameTableObjectRelationship = (
  relationship: AllowedTableRelationships,
): relationship is SameTableObjectRelationship => {
  if (!('using' in relationship)) return false;

  const constraint = relationship.using;

  if (!('foreign_key_constraint_on' in constraint)) return false;

  const foreign_key_constraint_on = constraint.foreign_key_constraint_on;

  if (!(
    typeof foreign_key_constraint_on === 'string' ||
    isArrayOfStrings(foreign_key_constraint_on)
  ))
    return false;

  // turn off "obj is declared but never used."
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  const sameTableObjectRelationshipConstraint: SameTableObjectRelationship['using']['foreign_key_constraint_on'] =
    foreign_key_constraint_on;

  return true;
};

export const isManualArrayRelationship = (
  relationship: AllowedTableRelationships,
): relationship is ManualArrayRelationship => {
  if (!('using' in relationship)) return false;

  const constraint = relationship.using;

  if (!('manual_configuration' in constraint)) return false;

  // turn off "obj is declared but never used."
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  const manualArrayRelationshipsConfig: ManualArrayRelationship['using'] =
    constraint;

  return true;
};
