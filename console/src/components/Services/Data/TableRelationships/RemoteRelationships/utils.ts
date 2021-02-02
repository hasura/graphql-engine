import {
  isInputObjectType,
  isInterfaceType,
  isEnumType,
  isObjectType,
  isScalarType,
  GraphQLSchema,
  GraphQLField,
  GraphQLType,
  GraphQLArgument,
  GraphQLInputField,
} from 'graphql';
import {
  isString,
  isObject,
  isFloat,
  isNumber,
} from '../../../../Common/utils/jsUtils';
import { getUnderlyingType } from '../../../../../shared/utils/graphqlSchemaUtils';
import { QualifiedTable } from '../../../../../metadata/types';

export type ArgValueKind = 'column' | 'static';
export type ArgValue = {
  kind: ArgValueKind;
  value: string;
  type: string;
};
export const defaultArgValue: ArgValue = {
  kind: 'column',
  value: '',
  type: 'String',
};

// Client Type
export type RemoteFieldArgument = {
  name: string;
  depth: number;
  parentField: string;
  parentFieldDepth: number;
  isChecked: boolean;
  parent?: string;
  value: ArgValue;
  type: string;
};
export interface TreeArgElement extends RemoteFieldArgument {
  isLeafArg: boolean;
  kind: 'argument';
}

export type RemoteField = {
  name: string;
  depth: number;
  parent?: string;
  arguments: RemoteFieldArgument[];
};
export interface TreeFieldElement extends Omit<RemoteField, 'arguments'> {
  kind: 'field';
  isChecked: boolean;
  enabled: boolean;
}

export type RJSchemaTreeElement = TreeArgElement | TreeFieldElement;

export type RemoteRelationship = {
  name: string;
  remoteSchema: string;
  remoteFields: RemoteField[];
  table: {
    name: string;
    schema: string;
  };
};

// Server Type
type RemoteRelationshipFieldServer = {
  field?: Record<string, RemoteRelationshipFieldServer>;
  arguments: Record<string, any>;
};
export type RemoteRelationshipServer = {
  remote_relationship_name: string;
  definition: {
    remote_field: Record<string, RemoteRelationshipFieldServer>;
    remote_schema: string;
    hasura_fields: string[];
  };
  table_name: string;
  table_schema: string;
};

export const parseArgValue = (argValue: any): ArgValue | null => {
  if (isObject(argValue)) {
    return null;
  }
  if (isString(argValue)) {
    const isStatic = !argValue.startsWith('$');
    return {
      value: isStatic ? argValue.toString() : argValue.substr(1),
      kind: isStatic ? 'static' : 'column',
      type: 'String',
    };
  }
  if (argValue === true || argValue === false) {
    return {
      kind: 'static',
      value: argValue.toString(),
      type: 'Boolean',
    };
  }
  return {
    kind: 'static',
    value: argValue.toString(),
    type:
      (isNumber(argValue) && (isFloat(argValue) ? 'Float' : 'Int')) || 'String',
  };
};

// Converters and parsers
const serialiseArguments = (
  args: Record<string, any>,
  depth: number,
  parentField: string,
  parentFieldDepth: number,
  parent?: string
): RemoteFieldArgument[] => {
  let allArgs: RemoteFieldArgument[] = [];
  Object.keys(args).forEach(argName => {
    const argValue = args[argName];
    const argValueMetadata = parseArgValue(argValue);
    if (argValueMetadata) {
      allArgs.push({
        name: argName,
        depth,
        parent,
        isChecked: false,
        parentField,
        parentFieldDepth,
        value: argValueMetadata,
        type: argValueMetadata.type,
      });
    } else {
      allArgs = [
        ...allArgs,
        {
          name: argName,
          depth,
          parent,
          isChecked: false,
          parentField,
          parentFieldDepth,
          value: defaultArgValue,
          type: 'String',
        },
        ...serialiseArguments(
          argValue,
          depth + 1,
          parentField,
          parentFieldDepth,
          argName
        ),
      ];
    }
  });
  return allArgs;
};

const serialiseRemoteField = (
  name: string,
  depth: number,
  field: RemoteRelationshipFieldServer,
  callback: (f: RemoteField) => void,
  parent?: string
): void => {
  callback({
    name,
    depth,
    parent,
    arguments: serialiseArguments(field.arguments, 0, name, depth, undefined),
  });
  if (field.field) {
    const subFieldName = Object.keys(field.field)[0];
    const subField = field.field[subFieldName];
    serialiseRemoteField(subFieldName, depth + 1, subField, callback, name);
  }
};

export const parseRemoteRelationship = (
  relationship: RemoteRelationshipServer
): RemoteRelationship => {
  const remoteField = relationship.definition.remote_field;
  const allRemoteFields: RemoteField[] = [];
  Object.keys(remoteField).forEach(fieldName => {
    serialiseRemoteField(
      fieldName,
      0,
      remoteField[fieldName],
      (field: RemoteField) => allRemoteFields.push(field),
      undefined
    );
  });
  return {
    name: relationship.remote_relationship_name,
    remoteSchema: relationship.definition.remote_schema,
    table: {
      name: relationship.table_name,
      schema: relationship.table_schema,
    },
    remoteFields: allRemoteFields,
  };
};

const getTypedArgValueInput = (argValue: ArgValue, type: string) => {
  let error: string | null = null;
  let value: any;

  if (argValue.kind === 'column') {
    return {
      value: `$${argValue.value}`,
      error,
    };
  }

  switch (type) {
    case 'Float': {
      const number = parseFloat(argValue.value);
      if (window.isNaN(number)) {
        error = 'invalid float value';
      }
      value = number;
      break;
    }
    case 'Int': {
      const number = parseInt(argValue.value, 10);
      if (window.isNaN(number)) {
        error = 'invalid int value';
      }
      value = number;
      break;
    }
    case 'ID': {
      const number = parseInt(argValue.value, 10);
      if (window.isNaN(number)) {
        error = 'invalid int value';
      }
      value = number;
      break;
    }
    case 'Boolean': {
      error =
        argValue.value.toLowerCase() !== 'false' &&
        argValue.value.toLowerCase() !== 'true'
          ? 'invalid boolean value'
          : null;
      if (!error) {
        value = argValue.value.toLowerCase() === 'true';
      }
      break;
    }
    default:
      value = argValue.value;
      break;
  }
  return {
    error,
    value,
  };
};

export type RemoteRelationshipPayload = {
  name: string;
  remote_schema: string;
  remote_field: Record<string, RemoteRelationshipFieldServer>;
  hasura_fields: string[];
  table: QualifiedTable;
};

export const getRemoteRelPayload = (
  relationship: RemoteRelationship
): RemoteRelationshipPayload => {
  const hasuraFields: string[] = [];
  const getRemoteFieldArguments = (field: RemoteField) => {
    const getArgumentObject = (depth: number, parent?: string) => {
      const depthArguments = field.arguments.filter(
        a => a.depth === depth && a.parent === parent
      );
      const finalArgObj: any = depthArguments.reduce(
        (argObj: any, currentArg) => {
          const nestedArgObj = getArgumentObject(depth + 1, currentArg.name);
          if (!nestedArgObj) {
            const argValueTyped = getTypedArgValueInput(
              currentArg.value,
              currentArg.type
            );
            if (argValueTyped.error) {
              throw Error(argValueTyped.error);
            }
            if (currentArg.value.kind === 'column') {
              hasuraFields.push(argValueTyped.value);
            }
            return {
              ...argObj,
              [currentArg.name]: argValueTyped.value,
            };
          }
          return {
            ...argObj,
            [currentArg.name]: nestedArgObj,
          };

          return {
            ...argObj,
          };
        },
        {}
      );
      return Object.keys(finalArgObj).length ? finalArgObj : undefined;
    };
    return getArgumentObject(0);
  };

  const getRemoteFieldObject = (
    depth: number
  ): Record<string, RemoteRelationshipFieldServer> | undefined => {
    const obj: Record<string, RemoteRelationshipFieldServer> = {};
    const depthRemoteFields = relationship.remoteFields.filter(
      f => f.depth === depth
    );
    depthRemoteFields.forEach(f => {
      obj[f.name] = {
        field: getRemoteFieldObject(depth + 1),
        arguments: getRemoteFieldArguments(f) || {},
      };
    });
    return Object.keys(obj).length ? obj : undefined;
  };

  return {
    name: relationship.name,
    remote_schema: relationship.remoteSchema,
    remote_field: getRemoteFieldObject(0) || {},
    hasura_fields: hasuraFields
      .map(f => f.substr(1))
      .filter((v, i, s) => s.indexOf(v) === i),
    table: relationship.table,
  };
};

const isFieldChecked = (
  relationship: RemoteRelationship,
  fieldName: string,
  depth: number,
  parent?: string
) => {
  return relationship.remoteFields.some(f => {
    return f.name === fieldName && f.depth === depth && f.parent === parent;
  });
};

/* returns checked value if arg is checked
 * returns null if arg isn't involved in the relationship
 */
export const getCheckedArgValue = (
  relationship: RemoteRelationship,
  argName: string,
  argType: GraphQLType,
  depth: number,
  parentField: string,
  parentFieldDepth: number,
  parent?: string
): ArgValue | null => {
  const parentRemoteField = relationship.remoteFields.find(f => {
    return f.name === parentField && f.depth === parentFieldDepth;
  });
  if (parentRemoteField) {
    const checkedArg = parentRemoteField.arguments.find(
      arg =>
        arg.name === argName && arg.depth === depth && arg.parent === parent
    );
    if (checkedArg) {
      return {
        ...checkedArg.value,
        type: getUnderlyingType(argType).type.name,
      };
    }
  }
  return null;
};

const buildArgElement = (
  relationship: RemoteRelationship,
  arg: GraphQLArgument | GraphQLInputField,
  depth: number,
  parentField: string,
  parentFieldDepth: number,
  callback: (fe: TreeArgElement) => void,
  parent?: string
) => {
  const { type: argType }: { type: GraphQLType } = getUnderlyingType(arg.type);
  const argValue = getCheckedArgValue(
    relationship,
    arg.name,
    argType,
    depth,
    parentField,
    parentFieldDepth,
    parent
  );
  const isLeafArg = isScalarType(argType) || isEnumType(argType);
  callback({
    name: arg.name,
    kind: 'argument',
    depth,
    type:
      (isLeafArg &&
        (isEnumType(argType)
          ? 'String'
          : getUnderlyingType(argType).type.name)) ||
      undefined,
    parent,
    parentField,
    parentFieldDepth,
    value: argValue || defaultArgValue,
    isChecked: !!argValue,
    isLeafArg,
  });
  if (!!argValue && (isInputObjectType(argType) || isInterfaceType(argType))) {
    const argFields = argType.getFields();
    Object.values(argFields).forEach(argField => {
      buildArgElement(
        relationship,
        argField,
        depth + 1,
        parentField,
        parentFieldDepth,
        callback,
        arg.name
      );
    });
  }
};

const buildFieldElement = (
  relationship: RemoteRelationship,
  field: GraphQLField<any, any, Record<string, any>>,
  depth: number,
  callback: (element: RJSchemaTreeElement) => void,
  parent?: string
) => {
  const { type: fieldType }: { type: GraphQLType } = getUnderlyingType(
    field.type
  );
  const isChecked = isFieldChecked(relationship, field.name, depth, parent);
  callback({
    name: field.name,
    kind: 'field',
    depth,
    parent,
    isChecked,
    enabled: (field.args && !!field.args.length) || isObjectType(fieldType),
  });
  if (isChecked) {
    if (field.args) {
      field.args.forEach(arg => {
        buildArgElement(relationship, arg, 0, field.name, depth, callback);
      });
    }
    if (isObjectType(fieldType) || isInterfaceType(fieldType)) {
      const subFields = fieldType.getFields();
      Object.values(subFields).forEach(subField => {
        buildFieldElement(
          relationship,
          subField,
          depth + 1,
          callback,
          field.name
        );
      });
    }
  }
};

export const buildSchemaTree = (
  relationship: RemoteRelationship,
  remoteSchema?: GraphQLSchema
): RJSchemaTreeElement[] => {
  if (!remoteSchema) return [];
  const schemaTree: (TreeArgElement | TreeFieldElement)[] = [];
  const queryType = remoteSchema.getQueryType();
  if (!queryType) return [];
  const fields = queryType.getFields();
  Object.values(fields).forEach(field => {
    buildFieldElement(
      relationship,
      field,
      0,
      (fieldElement: RJSchemaTreeElement) => schemaTree.push(fieldElement)
    );
  });
  return schemaTree;
};

export const compareRemoteFields = (
  rf1: RemoteField | TreeFieldElement,
  rf2: RemoteField | TreeFieldElement
) =>
  rf1.name === rf2.name && rf1.depth === rf2.depth && rf1.parent === rf2.parent;

export const compareRFArguments = (
  a1: RemoteFieldArgument,
  a2: RemoteFieldArgument
) =>
  a1.name === a2.name &&
  a1.depth === a2.depth &&
  a1.parent === a2.parent &&
  a1.parentField === a2.parentField &&
  a1.parentFieldDepth === a2.parentFieldDepth;

export const findRemoteFieldArgument = (
  args: RemoteFieldArgument[],
  arg: RemoteFieldArgument
) => {
  return args.find(a => compareRFArguments(a, arg));
};

export const findRemoteField = (
  fields: RemoteField[],
  field: RemoteField | TreeFieldElement
) => {
  return fields.find(
    f =>
      f.name === field.name &&
      f.depth === field.depth &&
      f.parent === field.parent
  );
};

export const findArgParentField = (
  fields: RemoteField[],
  arg: RemoteFieldArgument
) => {
  return fields.find(
    f => f.name === arg.parentField && f.depth === arg.parentFieldDepth
  );
};
