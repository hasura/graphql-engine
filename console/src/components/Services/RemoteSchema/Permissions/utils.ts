import {
  GraphQLEnumType,
  GraphQLInputObjectType,
  GraphQLNonNull,
  GraphQLObjectType,
  GraphQLScalarType,
  GraphQLSchema,
  parse,
  DocumentNode,
  ObjectFieldNode,
  FieldDefinitionNode,
  InputValueDefinitionNode,
  ArgumentNode,
  ObjectTypeDefinitionNode,
  GraphQLInputField,
  GraphQLList,
  GraphQLInputFieldMap,
  GraphQLEnumValue,
  GraphQLType,
} from 'graphql';
import { isJsonString } from '../../../Common/utils/jsUtils';
import {
  PermissionEdit,
  DatasourceObject,
  FieldType,
  argTreeType,
} from './types';

export const findRemoteSchemaPermission = (perms, role) => {
  return perms.find(p => p.role_name === role);
};

export const getCreateRemoteSchemaPermissionQuery = (
  def: { role: string },
  remoteSchemaName: string,
  schemaDefinition: string
) => {
  return {
    type: 'add_remote_schema_permissions',
    args: {
      remote_schema: remoteSchemaName,
      role: def.role,
      definition: {
        schema: schemaDefinition,
      },
    },
  };
};

export const getDropRemoteSchemaPermissionQuery = (
  role: string,
  remoteSchemaName: string
) => {
  return {
    type: 'drop_remote_schema_permissions',
    args: {
      remote_schema: remoteSchemaName,
      role,
    },
  };
};

export const getRemoteSchemaPermissionQueries = (
  permissionEdit: PermissionEdit,
  allPermissions: any,
  remoteSchemaName: string,
  schemaDefinition: string
) => {
  const { role, newRole } = permissionEdit;

  const upQueries = [];
  const downQueries = [];

  const permRole = (newRole || role).trim();

  const existingPerm = findRemoteSchemaPermission(allPermissions, permRole);
  // const existingPerm = allPermissions.find(p => p.role_name === permRole);

  if (newRole || (!newRole && !existingPerm)) {
    upQueries.push(
      getCreateRemoteSchemaPermissionQuery(
        {
          role: permRole,
        },
        remoteSchemaName,
        schemaDefinition
      )
    );
    downQueries.push(
      getDropRemoteSchemaPermissionQuery(permRole, remoteSchemaName)
    );
  }

  if (existingPerm) {
    upQueries.push(
      getDropRemoteSchemaPermissionQuery(permRole, remoteSchemaName)
    );
    upQueries.push(
      getCreateRemoteSchemaPermissionQuery(
        { role: permRole },
        remoteSchemaName,
        schemaDefinition
      )
    );
    downQueries.push(
      getDropRemoteSchemaPermissionQuery(permRole, remoteSchemaName)
    );
    downQueries.push(
      getCreateRemoteSchemaPermissionQuery(
        { role: permRole },
        remoteSchemaName,
        existingPerm.definition
      )
    );
  }

  return {
    upQueries,
    downQueries,
  };
};

export const updateBulkSelect = (
  bulkSelect: string[],
  selectedRole: string,
  isAdd: boolean
) => {
  const bulkRes = isAdd
    ? [...bulkSelect, selectedRole]
    : bulkSelect.filter(e => e !== selectedRole);
  return bulkRes;
};

export const getTree = (
  introspectionSchema: GraphQLSchema | null,
  permissionsSchema: GraphQLSchema | null,
  typeS: string
) => {
  const introspectionSchemaFields =
    typeS === 'QUERY'
      ? introspectionSchema!.getQueryType()?.getFields()
      : introspectionSchema!.getMutationType()?.getFields();

  let permissionsSchemaFields: any = null; // TODO use ternary operator
  if (permissionsSchema !== null) {
    permissionsSchemaFields =
      typeS === 'QUERY'
        ? permissionsSchema!.getQueryType()?.getFields()
        : permissionsSchema!.getMutationType()?.getFields();
  }

  if (introspectionSchemaFields) {
    return Object.values(introspectionSchemaFields).map(
      ({ name, args: argArray, type, ...rest }: any) => {
        let checked = true;
        const args = argArray.reduce((p: argTreeType, c: FieldType) => {
          return { ...p, [c.name]: { ...c } };
        }, {});
        if (permissionsSchema !== null && !(name in permissionsSchemaFields)) {
          checked = false;
        }
        return { name, checked, args, return: type.toString(), ...rest };
      }
    );
  }
  return [];
};

export const getType = (
  introspectionSchema: GraphQLSchema | null,
  permissionsSchema: GraphQLSchema | null
) => {
  const introspectionSchemaFields = introspectionSchema!.getTypeMap();
  // const permissionsSchemaFields =
  //   permissionsSchema !== null ? permissionsSchema!.getTypeMap() : null;
  let permissionsSchemaFields: any = null; // TODO use ternary operator
  if (permissionsSchema !== null) {
    permissionsSchemaFields = permissionsSchema!.getTypeMap();
  }

  const types: any[] = [];

  Object.entries(introspectionSchemaFields).forEach(([key, value]: any) => {
    if (
      !(
        value instanceof GraphQLObjectType ||
        value instanceof GraphQLInputObjectType
      )
    )
      return;

    const name = value.inspect();
    if (
      name === 'query_root' ||
      name === 'mutation_root' ||
      name === 'subscription_root'
    )
      return;

    if (name.includes('__')) return; // TODO change this check

    const type: DatasourceObject = { name: ``, children: [] };

    if (value instanceof GraphQLObjectType) {
      type.name = `type ${name}`;
    } else if (value instanceof GraphQLInputObjectType) {
      type.name = `input ${name}`;
    }

    const childArray: any[] = [];
    const fieldVal = value.getFields();
    let permissionsFieldVal: any = {};
    let isField = true;

    if (permissionsSchema !== null) {
      if (key in permissionsSchemaFields) {
        permissionsFieldVal = permissionsSchemaFields[key].getFields();
      } else {
        isField = false;
      }
    }

    Object.entries(fieldVal).forEach(([k, v]) => {
      let checked = true;
      if (
        permissionsSchema !== null &&
        (!(k in permissionsFieldVal) || !isField)
      ) {
        checked = false;
      }
      childArray.push({
        name: v.name,
        checked,
        return: v.type.toString(),
      });
    });

    type.children = childArray;
    types.push(type);
  });
  return types;
};

const getEnumTypes = (schema: GraphQLSchema) => {
  const fields = schema.getTypeMap();
  const types: any[] = [];
  Object.entries(fields).forEach(([, value]: any) => {
    if (!(value instanceof GraphQLEnumType)) return;
    const name = value.inspect();

    if (name.includes('__')) return; // TODO change this check

    const type: any = {};
    type.name = `enum ${name}`;

    const childArray: any[] = [];
    const fieldVal = value.getValues();

    Object.entries(fieldVal).forEach(([, v]) => {
      childArray.push({
        name: v.name,
        checked: true,
        return: v.value.toString(),
      });
    });

    type.children = childArray;
    types.push(type);
  });
  return types;
};

export const getScalarTypes = (schema: GraphQLSchema) => {
  const fields = schema.getTypeMap();
  const types: string[] = [];
  const gqlDefaultTypes = ['Boolean', 'Float', 'String', 'Int', 'ID'];
  Object.entries(fields).forEach(([, value]: any) => {
    if (!(value instanceof GraphQLScalarType)) return;
    const name = value.inspect();
    if (gqlDefaultTypes.indexOf(name) > -1) return; // Check if type belongs to default gql scalar types

    const type = `scalar ${name}`;
    types.push(type);
  });
  return types;
};

const checkNullType = (type: DatasourceObject) => {
  const isChecked = (element: FieldType) => element.checked;
  return type.children.some(isChecked);
};

// method that tells whether the field is nested or not, if nested it returns the children
export const getChildArguments = (v: GraphQLInputField): ChildArgumentType => {
  // TODO check if there are any more possible types with children / expandable views
  if (typeof v === 'string') return { children: null }; // value field
  if (v?.type instanceof GraphQLInputObjectType && v?.type?.getFields)
    return {
      children: v?.type?.getFields(),
      path: 'type._fields',
      childrenType: v?.type,
    };

  // 1st order
  // either list or nonNull
  if (
    (v?.type instanceof GraphQLNonNull || v?.type instanceof GraphQLList) &&
    v?.type?.ofType &&
    v?.type?.ofType?._fields
  ) {
    return {
      children: v?.type?.ofType?._fields as GraphQLInputFieldMap,
      path: 'type.ofType',
      childrenType: v?.type?.ofType,
    };
  }

  // 2nd Order
  // nonNull inside a GQL list or list inside a nonNull
  if (
    (v?.type instanceof GraphQLList || v?.type instanceof GraphQLNonNull) &&
    v?.type?.ofType
  )
    if (
      (v?.type?.ofType instanceof GraphQLNonNull ||
        v?.type?.ofType instanceof GraphQLList) &&
      v?.type?.ofType &&
      v?.type?.ofType?.ofType
    ) {
      const type = v?.type?.ofType;
      if (type?.ofType instanceof GraphQLEnumType)
        return {
          children: type?.ofType?.getValues() as GraphQLEnumValue[],
          path: 'type.ofType.ofType',
          childrenType: type?.ofType,
        };
      if (type?.ofType instanceof GraphQLInputObjectType)
        return {
          children: type?.ofType?.getFields() as GraphQLInputFieldMap,
          childrenType: type?.ofType,
          path: 'type.ofType.ofType',
        };
    }

  // 3rd Order example: ![!user_type]
  if (
    (v?.type instanceof GraphQLList || v?.type instanceof GraphQLNonNull) &&
    v?.type?.ofType
  )
    if (
      (v?.type?.ofType instanceof GraphQLNonNull ||
        v?.type?.ofType instanceof GraphQLList) &&
      v?.type?.ofType &&
      v?.type?.ofType?.ofType
    )
      if (
        (v?.type?.ofType?.ofType instanceof GraphQLNonNull ||
          v?.type?.ofType?.ofType instanceof GraphQLList) &&
        v?.type?.ofType?.ofType &&
        v?.type?.ofType?.ofType?.ofType
      ) {
        const type = v?.type?.ofType?.ofType;
        if (type?.ofType instanceof GraphQLEnumType)
          return {
            children: type?.ofType?.getValues() as GraphQLEnumValue[],
            path: 'type.ofType.ofType.ofType',
            childrenType: type?.ofType,
          };
        if (type?.ofType instanceof GraphQLInputObjectType)
          return {
            children: type?.ofType?.getFields() as GraphQLInputFieldMap,
            childrenType: type?.ofType,
            path: 'type.ofType.ofType.ofType',
          };
      }
  return {};
};

// arg => {id:{_eq:1}}
// argDef => GQL type
const serialiseArgs = (args, argDef) => {
  // console.log(args, argDef);
  let res = '{';
  const { children } = getChildArguments(argDef);
  Object.entries(args).forEach(([key, value]) => {
    if (!value) return;

    // console.log({ key, value, children, childrenType });
    if (value && typeof value === 'string') {
      let val;
      if (
        children[key] &&
        children[key].type instanceof GraphQLEnumType &&
        !value.startsWith('x-hasura')
      ) {
        val = `${key}:${value}`; // no double quotes
      } else {
        val = `${key}:"${value}"`;
      }

      if (res === '{') {
        res = `${res} ${val}`;
      } else {
        res = `${res} , ${val}`;
      }
    } else if (value && typeof value === 'object') {
      if (children && typeof children === 'object' && children[key])
        res = `${res} ${key}: ${serialiseArgs(value, children[key])}`;
    }
  });
  return `${res}}`;
  // // console.log(args)
  // window.getChildArguments = getChildArguments;
  // // console.log(getChildArguments(argDef))
  // return JSON.stringify(args);
};

const getSDLField = (
  type: DatasourceObject,
  argTree: Record<string, any> | null
) => {
  if (!checkNullType(type)) return '';

  let result = ``;
  const typeName: string = type.name;
  if (type.name === 'query_root' || type.name === 'mutation_root')
    result = `type ${typeName}{`;
  else result = `${typeName}{`;

  type.children.forEach(f => {
    // TODO filter selected fields
    if (!f.checked) return null;

    let fieldStr = f.name;

    // this will process all types except enums, enums are processed seperately
    if (!typeName.includes('enum')) {
      if (f?.args) {
        fieldStr = `${fieldStr}(`;
        // console.log('f.args >>>'   ,f.args);
        Object.values(f.args).forEach((arg: any) => {
          let valueStr = ``;
          if (argTree && argTree[f.name] && argTree[f.name][arg.name]) {
            const argName = argTree[f.name][arg.name];
            const typeOfArg = arg.type.inspect();
            let unquoted;
            let isSessionVar = false;

            if (typeof argName === 'string') {
              isSessionVar = argName.startsWith('x-hasura');
            }

            if (
              typeOfArg === 'string' ||
              isSessionVar ||
              typeof argName === 'object'
            ) {
              unquoted = serialiseArgs(argName, arg);
              // const jsonStr = JSON.stringify(argName);
              // unquoted = jsonStr.replace(/"([^"]+)":/g, '$1:');
              // console.log(argTree, f, arg, f.name, arg.name);
            } else {
              unquoted = argName;
            }

            valueStr = `${arg.name} : ${arg.type.inspect()}
            @preset(value: ${unquoted})`;
          } else {
            valueStr = `${arg.name} : ${arg.type.inspect()}`;
          }
          fieldStr = `${fieldStr + valueStr} `;
        });
        fieldStr = `${fieldStr})`;
        fieldStr = `${fieldStr}: ${f.return}`;
      } else fieldStr = `${fieldStr} : ${f.return}`; // normal data type - ie: without arguments/ presets
    }

    result = `${result}
      ${fieldStr}`;
  });
  return `${result}\n}`;
};

export const generateConstantTypes = (schema: GraphQLSchema): string => {
  let result = ``;
  const enumTypes = getEnumTypes(schema);
  enumTypes.forEach(type => {
    result = `${result}\n${getSDLField(type, null)}`;
  });
  const scalarTypes = getScalarTypes(schema);
  scalarTypes.forEach(type => {
    result = `${result}\n${type}`;
  });

  return result;
};

export const generateSDL = (
  types: DatasourceObject[],
  argTree: Record<string, any>
) => {
  let result = `schema{
  query: query_root
  mutation: mutation_root
}\n`;
  types.forEach(type => {
    result = `${result}\n${getSDLField(type, argTree)}\n`;
  });

  return result;
};
type ChildArgumentType = {
  children?: GraphQLInputFieldMap | GraphQLEnumValue[] | null;
  path?: string;
  childrenType?: GraphQLType;
};

// TODO request to add this change on the server.
export const addPresetDefinition = (schema: string) => `scalar PresetValue\n
  directive @preset(
      value: PresetValue
  ) on INPUT_FIELD_DEFINITION | ARGUMENT_DEFINITION\n
${schema}`;

const parseObjectField = (arg: ArgumentNode | ObjectFieldNode) => {
  if (arg?.value?.kind === 'IntValue' && arg?.value?.value)
    return arg?.value?.value;
  if (arg?.value?.kind === 'FloatValue' && arg?.value?.value)
    return arg?.value?.value;
  if (arg?.value?.kind === 'StringValue' && arg?.value?.value)
    return arg?.value?.value;
  if (arg?.value?.kind === 'BooleanValue' && arg?.value?.value)
    return arg?.value?.value;
  if (arg?.value?.kind === 'EnumValue' && arg?.value?.value)
    return arg?.value?.value;

  if (arg?.value?.kind === 'NullValue') return null;

  // nested values
  if (
    arg?.value?.kind === 'ObjectValue' &&
    arg?.value?.fields &&
    arg?.value?.fields?.length > 0
  ) {
    const res: Record<string, any> = {};
    arg?.value?.fields.forEach((f: ObjectFieldNode) => {
      res[f.name.value] = parseObjectField(f);
    });
    return res;
  }
};

const getDirectives = (field: InputValueDefinitionNode) => {
  let res: unknown | Record<string, any>;
  const preset = field?.directives?.find(dir => dir?.name?.value === 'preset');
  if (preset?.arguments && preset?.arguments[0])
    res = parseObjectField(preset.arguments[0]);
  if (typeof res === 'object') return res;
  if (typeof res === 'string' && isJsonString(res)) return JSON.parse(res);
  return res;
};

const getPresets = (field: FieldDefinitionNode) => {
  const res: Record<string, any> = {};
  field?.arguments?.forEach(arg => {
    if (arg.directives && arg.directives.length > 0)
      res[arg?.name?.value] = getDirectives(arg);
  });
  return res;
};

const getFieldsMap = (fields: FieldDefinitionNode[]) => {
  const res: Record<string, any> = {};
  fields.forEach(field => {
    res[field?.name?.value] = getPresets(field);
  });
  return res;
};

export const getArgTreeFromPermissionSDL = (definition: string) => {
  const roots = ['query_root', 'mutation_root'];
  try {
    const schema: DocumentNode = parse(definition);
    const defs = schema.definitions as ObjectTypeDefinitionNode[];
    const argTree =
      defs &&
      defs.reduce((acc = [], i) => {
        if (i.name && i.fields && roots.includes(i?.name?.value)) {
          const res = getFieldsMap(i.fields as FieldDefinitionNode[]);
          return { ...acc, ...res };
        }
        return acc;
      }, {});
    return argTree;
  } catch (e) {
    console.error(e);
    return null;
  }
};

export const generateTypeString = (str: string) => str.replace(/[^\w\s]/gi, '');
