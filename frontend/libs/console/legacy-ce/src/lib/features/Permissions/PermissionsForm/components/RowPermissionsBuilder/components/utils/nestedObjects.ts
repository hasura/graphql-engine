import { getNamedType, getNullableType, isObjectType } from 'graphql';
import { Columns } from '../types';

export function fieldsToColumns(column: Columns[number] | undefined): Columns {
  if (!column) return [];
  const type = column.graphQLProperties?.graphQLType;
  if (!type) return [];
  const nullableNamedType = getNamedType(getNullableType(type));
  if (!isObjectType(nullableNamedType)) return [];
  const fields = nullableNamedType.getFields();
  return Object.entries(fields).map(([name, field]) => ({
    name,
    dataType: field.type.toString(),
    graphQLProperties: {
      graphQLType: field.type,
      name: field.name,
      scalarType: field.type.toString(),
    },
  }));
}
