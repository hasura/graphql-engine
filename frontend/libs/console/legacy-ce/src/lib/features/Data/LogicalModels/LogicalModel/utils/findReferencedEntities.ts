import {
  LogicalModelField,
  Source,
  isArrayLogicalModelType,
  isLogicalModelType,
} from '../../../../hasura-metadata-types';

// returns true if field implements logical model by name
export const isFieldImplementingLogicalModel = (
  field: LogicalModelField,
  nameToMatch: string
) =>
  (isLogicalModelType(field.type) &&
    field.type.logical_model === nameToMatch) ||
  // match a logical model where the field is an array of THIS logical model
  (isArrayLogicalModelType(field.type) &&
    field.type.array.logical_model === nameToMatch);

export const findReferencedEntities = ({
  source,
  logicalModelName,
}: {
  source: Source | undefined;
  logicalModelName: string;
}) => {
  const entities = {
    stored_procedures: (source?.stored_procedures ?? []).filter(
      p => p.returns === logicalModelName
    ),
    native_queries: (source?.native_queries ?? []).filter(
      n => n.returns === logicalModelName
    ),
    logical_models: (source?.logical_models ?? [])
      // first, narrow to only logical models whose fields reference THIS logical model
      .filter(m =>
        m.fields.some(field =>
          isFieldImplementingLogicalModel(field, logicalModelName)
        )
      )
      // then, return those models, as well as a list of the matching fields
      .map(m => ({
        logicalModel: m,
        matchingFields: m.fields.filter(field =>
          isFieldImplementingLogicalModel(field, logicalModelName)
        ),
      })),
    // schemaless tables need a link to a logical model:
    tables: (source?.tables ?? []).filter(table => {
      return (
        'logical_model' in table && table.logical_model === logicalModelName
      );
    }),
  };

  return {
    ...entities,
    count: Object.entries(entities).reduce<number>((sum, [, entry]) => {
      return sum + entry.length;
    }, 0),
  };
};
