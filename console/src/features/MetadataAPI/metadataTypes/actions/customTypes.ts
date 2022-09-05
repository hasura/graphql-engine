import { Table } from '../source';
import { GraphQLName, GraphQLType } from './actions';

type RelationshipName = string;

interface CustomTypeObjectRelationship {
  /** Name of the relationship, shouldn’t conflict with existing field names */
  name: RelationshipName;
  /** Type of the relationship */
  type: 'object' | 'array';
  /** The table to which relationship is defined */
  remote_table: Table;
  /** Mapping of fields of object type to columns of remote table  */
  field_mapping: {
    [ObjectFieldName: string]: string;
  };
  /** Source name, where remote_table exists */
  source?: string;
}

interface InputObjectField {
  /** Name of the Input object type */
  name: GraphQLName;
  /** Description of the Input object type */
  description?: string;
  /** GraphQL type of the Input object type */
  type: GraphQLType;
}

interface InputObjectType {
  /** Name of the Input object type */
  name: GraphQLName;
  /** Description of the Input object type */
  description?: string;
  /** Fields of the Input object type */
  fields: InputObjectField[];
}

interface ObjectType {
  /** Name of the Input object type */
  name: GraphQLName;
  /** Description of the Input object type */
  description?: string;
  /** Fields of the Input object type */
  fields: InputObjectField[];
  /** Relationships of the Object type to tables */
  relationships?: CustomTypeObjectRelationship[];
}

interface ScalarType {
  /** Name of the Scalar type */
  name: GraphQLName;
  /** Description of the Scalar type */
  description?: string;
}

interface EnumType {
  /** Name of the Enum type */
  name: GraphQLName;
  /** Description of the Enum type */
  description?: string;
  /** Values of the Enum type */
  values: EnumValue[];
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-types.html#enumvalue
 */
interface EnumValue {
  /** Value of the Enum type */
  value: GraphQLName;
  /** Description of the Enum value */
  description?: string;
  /** If set to true, the enum value is marked as deprecated */
  is_deprecated?: boolean;
}

export interface CustomTypes {
  input_objects?: InputObjectType[];
  objects?: ObjectType[];
  scalars?: ScalarType[];
  enums?: EnumType[];
}
