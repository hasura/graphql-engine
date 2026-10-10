import {
  GraphQLField,
  GraphQLArgument,
  GraphQLInputFieldMap,
  GraphQLEnumValue,
  GraphQLType,
} from 'graphql';

export type PermOpenEditType = (
  role: string,
  newRole: boolean,
  existingPerms: boolean,
) => void;

export type ArgTreeType = {
  [key: string]: string | number | ArgTreeType;
};

export type ChildArgumentType = {
  children?: GraphQLInputFieldMap | GraphQLEnumValue[];
  path?: string;
  childrenType?: GraphQLType;
};

export type CustomFieldType = {
  name: string;
  checked: boolean;
  args?: Record<string, GraphQLArgument>;
  return?: string;
  typeName?: string;
  children?: FieldType[];
  defaultValue?: any;
  isInputObjectType?: boolean;
  parentName?: string;
};

export type FieldType = CustomFieldType & GraphQLField<any, any>;

export type RemoteSchemaFields =
  | {
      name: string;
      typeName: string;
      children: FieldType[] | CustomFieldType[];
    }
  | FieldType;

export type ExpandedItems = {
  [key: string]: boolean;
};
