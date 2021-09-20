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
  GraphQLFieldMap,
  ValueNode,
  GraphQLInputType,
  buildSchema,
  GraphQLUnionType,
  InputObjectTypeDefinitionNode,
} from 'graphql';
import {
  isJsonString,
  isEmpty,
  isArrayString,
} from '../../../Common/utils/jsUtils';
import {
  PermissionEdit,
  RemoteSchemaFields,
  FieldType,
  ArgTreeType,
  PermissionsType,
  CustomFieldType,
  ChildArgumentType,
  ExpandedItems,
} from './types';
import Migration from '../../../../utils/migration/Migration';

export const findRemoteSchemaPermission = (
  perms: PermissionsType[],
  role: string
) => {
  return perms.find(p => p.role === role);
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
  allPermissions: PermissionsType[],
  remoteSchemaName: string,
  schemaDefinition: string
) => {
  const { role, newRole } = permissionEdit;

  const permRole = (newRole || role).trim();

  const existingPerm = findRemoteSchemaPermission(allPermissions, permRole);
  const migration = new Migration();

  if (newRole || (!newRole && !existingPerm)) {
    migration.add(
      getCreateRemoteSchemaPermissionQuery(
        {
          role: permRole,
        },
        remoteSchemaName,
        schemaDefinition
      ),
      getDropRemoteSchemaPermissionQuery(permRole, remoteSchemaName)
    );
  }

  if (existingPerm) {
    migration.add(
      getDropRemoteSchemaPermissionQuery(permRole, remoteSchemaName),
      getDropRemoteSchemaPermissionQuery(permRole, remoteSchemaName)
    );
    migration.add(
      getCreateRemoteSchemaPermissionQuery(
        { role: permRole },
        remoteSchemaName,
        schemaDefinition
      ),
      getCreateRemoteSchemaPermissionQuery(
        { role: permRole },
        remoteSchemaName,
        existingPerm.definition.schema
      )
    );
  }

  return {
    upQueries: migration.upMigration,
    downQueries: migration.downMigration,
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

/**
 * Sets query_root and mutation_root in UI tree.
 * @param introspectionSchema Remote Schema introspection schema.
 * @param permissionsSchema Permissions coming from saved role.
 * @param typeS Type of args.
 * @returns Array of schema fields (query_root and mutation_root)
 */
export const getTree = (
  introspectionSchema: GraphQLSchema | null,
  permissionsSchema: GraphQLSchema | null,
  typeS: string
) => {
  const introspectionSchemaFields =
    typeS === 'QUERY'
      ? introspectionSchema!.getQueryType()?.getFields()
      : introspectionSchema!.getMutationType()?.getFields();

  let permissionsSchemaFields:
    | GraphQLFieldMap<any, any, Record<string, any>>
    | null
    | undefined = null;
  if (permissionsSchema !== null) {
    permissionsSchemaFields =
      typeS === 'QUERY'
        ? permissionsSchema!.getQueryType()?.getFields()
        : permissionsSchema!.getMutationType()?.getFields();
  }

  if (introspectionSchemaFields) {
    return Object.values(introspectionSchemaFields).map(
      ({ name, args: argArray, type, ...rest }: any) => {
        let checked = false;
        const parentName =
          typeS === 'QUERY'
            ? `type ${introspectionSchema?.getQueryType()?.name}`
            : `type ${introspectionSchema?.getMutationType()?.name}`;
        const args = argArray.reduce((p: ArgTreeType, c: FieldType) => {
          return { ...p, [c.name]: { ...c } };
        }, {});
        if (
          permissionsSchema !== null &&
          permissionsSchemaFields &&
          name in permissionsSchemaFields
        ) {
          checked = true;
        }
        return {
          name,
          checked,
          args,
          return: type.toString(),
          parentName,
          ...rest,
        };
      }
    );
  }
  return [];
};

export const getSchemaRoots = (schema: GraphQLSchema) => {
  if (!schema) return [];
  const res = [schema.getQueryType()?.name]; // query root will be always present
  if (schema.getMutationType()?.name) res.push(schema.getMutationType()?.name);
  if (schema.getSubscriptionType()?.name)
    res.push(schema.getSubscriptionType()?.name);
  return res;
};

/**
 * Sets input types, object types, scalar types and enum types in UI tree.
 * @param introspectionSchema - Remote schema introspection schema.
 * @param permissionsSchema - Permissions coming from saved role.
 * @returns Array of all types
 */
export const getType = (
  introspectionSchema: GraphQLSchema | null,
  permissionsSchema: GraphQLSchema | null
) => {
  const introspectionSchemaFields = introspectionSchema!.getTypeMap();

  let permissionsSchemaFields: any = null;
  if (permissionsSchema !== null) {
    permissionsSchemaFields = permissionsSchema!.getTypeMap();
  }

  const enumTypes: RemoteSchemaFields[] = [];
  const scalarTypes: RemoteSchemaFields[] = [];
  const inputObjectTypes: RemoteSchemaFields[] = [];
  const objectTypes: RemoteSchemaFields[] = [];
  const unionTypes: RemoteSchemaFields[] = [];

  Object.entries(introspectionSchemaFields).forEach(([key, value]: any) => {
    if (
      !(
        value instanceof GraphQLObjectType ||
        value instanceof GraphQLInputObjectType ||
        value instanceof GraphQLEnumType ||
        value instanceof GraphQLScalarType ||
        value instanceof GraphQLUnionType
      )
    )
      return;

    const name = value.inspect();
    const roots = introspectionSchema
      ? getSchemaRoots(introspectionSchema)
      : [];

    if (roots.includes(name)) return;

    if (name.startsWith('__')) return;

    const type: RemoteSchemaFields = {
      name: ``,
      typeName: ``,
      children: [],
    };
    type.typeName = name;

    if (value instanceof GraphQLEnumType) {
      type.name = `enum ${name}`;
      const values = value.getValues();
      const childArray: CustomFieldType[] = [];
      let checked = false;
      if (
        permissionsSchema !== null &&
        permissionsSchemaFields !== null &&
        key in permissionsSchemaFields
      )
        checked = true;
      values.forEach(val => {
        childArray.push({
          name: val.name,
          checked,
        });
      });
      type.children = childArray;
      enumTypes.push(type);
    } else if (value instanceof GraphQLScalarType) {
      type.name = `scalar ${name}`;
      let checked = false;
      if (
        permissionsSchema !== null &&
        permissionsSchemaFields !== null &&
        key in permissionsSchemaFields
      )
        checked = true;
      const childArray: CustomFieldType[] = [{ name: type.name, checked }];
      type.children = childArray;
      scalarTypes.push(type);
    } else if (value instanceof GraphQLObjectType) {
      type.name = `type ${name}`;
    } else if (value instanceof GraphQLInputObjectType) {
      type.name = `input ${name}`;
    }

    if (
      value instanceof GraphQLObjectType ||
      value instanceof GraphQLInputObjectType
    ) {
      const childArray: CustomFieldType[] = [];
      const fieldVal = value.getFields();
      let permissionsFieldVal: GraphQLFieldMap<any, any, any> = {};
      let isFieldPresent = true;

      // Check if the type is present in the permission schema coming from user.
      if (permissionsSchema !== null && permissionsSchemaFields !== null) {
        if (key in permissionsSchemaFields) {
          permissionsFieldVal = permissionsSchemaFields[key].getFields();
        } else {
          isFieldPresent = false;
        }
      }

      // Checked is true when type is present and the fields are present in type
      Object.entries(fieldVal).forEach(([k, v]) => {
        let checked = false;
        if (
          permissionsSchema !== null &&
          isFieldPresent &&
          k in permissionsFieldVal
        ) {
          checked = true;
        }
        const field: CustomFieldType = {
          name: v.name,
          checked,
          return: v.type.toString(),
        };
        if (v.defaultValue !== undefined) {
          field.defaultValue = v.defaultValue;
        }
        if (value instanceof GraphQLInputObjectType) {
          field.args = { [k]: v };
          field.isInputObjectType = true;
          field.parentName = type.name;
        }
        childArray.push(field);
      });

      type.children = childArray;
      if (value instanceof GraphQLObjectType) objectTypes.push(type);
      if (value instanceof GraphQLInputObjectType) inputObjectTypes.push(type);
    }

    if (value instanceof GraphQLUnionType) {
      let isFieldPresent = true;
      let permissionsTypesVal: any;

      // Check if the type is present in the permission schema coming from user.
      if (permissionsSchema !== null && permissionsSchemaFields !== null) {
        if (key in permissionsSchemaFields) {
          permissionsTypesVal = permissionsSchemaFields[key].getTypes();
        } else {
          isFieldPresent = false;
        }
      }

      type.name = `union ${name}`;
      const childArray: CustomFieldType[] = [];
      const typesVal = value.getTypes();
      Object.entries(typesVal).forEach(([k, v]) => {
        let checked = false;
        if (
          permissionsSchema !== null &&
          isFieldPresent &&
          k in permissionsTypesVal
        ) {
          checked = true;
        }
        const field: CustomFieldType = {
          name: v.name,
          checked,
          return: v.name,
        };
        childArray.push(field);
      });

      type.children = childArray;
      unionTypes.push(type);
    }
  });
  return [
    ...objectTypes,
    ...inputObjectTypes,
    ...unionTypes,
    ...enumTypes,
    ...scalarTypes,
  ];
};

export const getRemoteSchemaFields = (
  schema: GraphQLSchema,
  permissionsSchema: GraphQLSchema | null
): RemoteSchemaFields[] => {
  const types = getType(schema, permissionsSchema);

  const queryRoot = schema?.getQueryType()?.name;
  const mutationRoot = schema?.getMutationType()?.name;

  const remoteFields = [
    {
      name: `type ${queryRoot}`,
      typeName: '__query_root',
      children: getTree(schema, permissionsSchema, 'QUERY'),
    },
  ];
  if (mutationRoot) {
    remoteFields.push({
      name: `type ${mutationRoot}`,
      typeName: '__mutation_root',
      children: getTree(schema, permissionsSchema, 'MUTATION'),
    });
  }
  return [...remoteFields, ...types];
};

// method that tells whether the field is nested or not, if nested it returns the children
export const getChildArguments = (v: GraphQLInputField): ChildArgumentType => {
  if (typeof v === 'string') return {}; // value field
  if (v?.type instanceof GraphQLInputObjectType && v?.type?.getFields)
    return {
      children: v?.type?.getFields(),
      path: 'type._fields',
      childrenType: v?.type,
    };

  // 1st order
  if (v?.type instanceof GraphQLNonNull || v?.type instanceof GraphQLList) {
    const children = getChildArguments({
      type: v?.type.ofType,
    } as GraphQLInputField).children;
    if (isEmpty(children)) return {};

    return {
      children,
      path: 'type.ofType',
      childrenType: v?.type?.ofType,
    };
  }

  return {};
};

const isList = (gqlArg: GraphQLInputField, value: string) =>
  gqlArg &&
  gqlArg.type instanceof GraphQLList &&
  typeof value === 'string' &&
  isArrayString(value) &&
  !value.toLowerCase().startsWith('x-hasura');

// utility function for getSDLField
const serialiseArgs = (args: ArgTreeType, argDef: GraphQLInputField) => {
  let res = '{';
  const { children } = getChildArguments(argDef);
  Object.entries(args).forEach(([key, value]) => {
    if (isEmpty(value) || isEmpty(children)) {
      return;
    }
    const gqlArgs = children as GraphQLInputFieldMap;
    const gqlArg = gqlArgs[key];

    if (typeof value === 'string' || typeof value === 'number') {
      let val;

      const isEnum =
        gqlArg &&
        gqlArg.type instanceof GraphQLEnumType &&
        typeof value === 'string' &&
        !value.toLowerCase().startsWith('x-hasura');

      switch (true) {
        case isEnum:
          val = `${key}:${value}`; // no double quotes
          break;
        case typeof value === 'number':
          val = `${key}: ${value} `;
          break;

        case typeof value === 'string' && isList(gqlArg, value):
          val = `${key}: ${value} `;
          break;

        default:
          val = `${key}:"${value}"`;
          break;
      }

      if (res === '{') {
        res = `${res} ${val}`;
      } else {
        res = `${res} , ${val}`;
      }
    } else if (value && typeof value === 'object') {
      if (children && typeof children === 'object' && gqlArg) {
        const valString = serialiseArgs(value, gqlArg);
        if (valString && res === '{') res = `${res} ${key}: ${valString}`;
        else if (valString) res = `${res} , ${key}: ${valString}`;
      }
    }
  });
  if (res === `{`) return; // dont return string when there is no value
  return `${res}}`;
};

const isEnumType = (type: GraphQLInputType): boolean => {
  if (type instanceof GraphQLList || type instanceof GraphQLNonNull)
    return isEnumType(type.ofType);
  else if (type instanceof GraphQLEnumType) return true;
  return false;
};

// Check if type belongs to default gql scalar types
const checkDefaultGQLScalarType = (typeName: string): boolean => {
  const gqlDefaultTypes = ['Boolean', 'Float', 'String', 'Int', 'ID'];
  if (gqlDefaultTypes.indexOf(typeName) > -1) return true;
  return false;
};

const checkEmptyType = (type: RemoteSchemaFields) => {
  const isChecked = (element: FieldType | CustomFieldType) => element.checked;
  if (type.children) return type.children.some(isChecked);
};

/**
 * Builds the SDL string for each field / type.
 * @param type - Data source object containing a schema field.
 * @param argTree - Arguments tree in case of types with argument presets.
 * @returns SDL string for passed field.
 */
const getSDLField = (
  type: RemoteSchemaFields,
  argTree: Record<string, any> | null
): string => {
  if (!checkEmptyType(type)) return ''; // check if no child is selected for a type

  let result = ``;
  const typeName: string = type.name;

  // add scalar fields to SDL
  if (typeName.startsWith('scalar')) {
    if (type.typeName && checkDefaultGQLScalarType(type.typeName))
      return result; // if default GQL scalar type, return empty string
    result = `${typeName}`;
    return `${result}\n`;
  }

  // add union fields to SDL
  if (typeName.startsWith('union') && type.children) {
    result = `${typeName} =`;
    type.children.forEach(t => {
      if (t.checked) {
        result = `${result} ${t.name} |`;
      }
    });
    result = result.substring(0, result.length - 1);
    return `${result}\n`;
  }

  // add other fields to SDL
  result = `${typeName}{`;

  if (type.children)
    type.children.forEach(f => {
      if (!f.checked) return null;

      let fieldStr = f.name;
      let valueStr = '';

      // enum types don't have args
      if (!typeName.startsWith('enum')) {
        if (f.args && !isEmpty(f.args)) {
          fieldStr = `${fieldStr}(`;
          Object.values(f.args).forEach((arg: GraphQLInputField) => {
            valueStr = `${arg.name} : ${arg.type.inspect()}`;

            if (argTree?.[type?.name]?.[f?.name]?.[arg?.name]) {
              const argName = argTree[type.name][f.name][arg.name];
              let unquoted;
              const isEnum =
                typeof argName === 'string' &&
                argName &&
                !argName.toLowerCase().startsWith('x-hasura') &&
                isEnumType(arg.type);

              if (typeof argName === 'object') {
                unquoted = serialiseArgs(argName, arg);
              } else if (typeof argName === 'number') {
                unquoted = `${argName}`;
              } else if (isEnum) {
                unquoted = `${argName}`;
              } else {
                unquoted = `"${argName}"`;
              }

              if (!isEmpty(unquoted))
                valueStr = `${valueStr} @preset(value: ${unquoted})`;
            }

            fieldStr = `${fieldStr + valueStr} `;
          });
          fieldStr = `${fieldStr})`;
          fieldStr = `${fieldStr}: ${f.return}`;
        } else {
          // normal data type - ie: without arguments/ presets
          fieldStr =
            f.defaultValue === undefined
              ? `${fieldStr} : ${f.return}`
              : `${fieldStr} : ${f.return} = ${f.defaultValue}`;
        }
      }
      // only need the arg string for input object types
      if (typeName.startsWith('input')) {
        result = `${result}
      ${valueStr}`;
      } else {
        result = `${result}
      ${fieldStr}`;
      }
    });
  return `${result}\n}`;
};

/**
 * Generate SDL string having input types and object types.
 * @param types - Remote schema introspection schema.
 * @returns String having all enum types and scalar types.
 */
export const generateSDL = (
  types: RemoteSchemaFields[] | FieldType[],
  argTree: ArgTreeType
) => {
  let prefix = `schema{`;
  let result = '';

  types.forEach(type => {
    const fieldDef = getSDLField(type, argTree);

    if (!isEmpty(fieldDef) && type.typeName === '__query_root' && type.name) {
      const name = type.name.split(' ')[1];
      prefix = `${prefix}
      query: ${name}`;
    }
    if (
      !isEmpty(fieldDef) &&
      type.typeName === '__mutation_root' &&
      type.name
    ) {
      const name = type.name.split(' ')[1];

      prefix = `${prefix}
      mutation: ${name}`;
    }
    if (!isEmpty(fieldDef)) result = `${result}\n${fieldDef}\n`;
  });

  prefix =
    prefix === `schema{`
      ? ''
      : `${prefix}
}\n`;

  if (isEmpty(result)) return '';

  return `${prefix} ${result}`;
};

export const addPresetDefinition = (schema: string) => `scalar PresetValue\n
  directive @preset(
      value: PresetValue
  ) on INPUT_FIELD_DEFINITION | ARGUMENT_DEFINITION\n
${schema}`;

export const buildSchemaFromRoleDefn = (roleDefinition: string) => {
  let permissionsSchema: GraphQLSchema | null = null;

  try {
    const newDef = addPresetDefinition(roleDefinition);
    permissionsSchema = buildSchema(newDef);
  } catch (err) {
    return null;
  }
  return permissionsSchema;
};

const addToArrayString = (acc: string, newStr: unknown, withQuotes = false) => {
  if (acc !== '') {
    if (withQuotes) acc = `${acc}, "${newStr}"`;
    else acc = `${acc}, ${newStr}`;
  } else acc = `[${newStr}`;
  return acc;
};

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

  // Array values
  if (
    arg?.value?.kind === 'ListValue' &&
    arg?.value?.values &&
    arg?.value?.values?.length > 0
  ) {
    let res = '';
    arg.value.values.forEach((v: ValueNode) => {
      if (v.kind === 'IntValue' || v.kind === 'FloatValue') {
        res = addToArrayString(res, v.value);
      } else if (v.kind === 'BooleanValue') {
        res = addToArrayString(res, v.value);
      } else if (v.kind === 'StringValue') {
        res = addToArrayString(res, v.value);
      }
    });
    return `${res}]`;
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

const getFieldsMap = (fields: FieldDefinitionNode[], parentName: string) => {
  const type = `type ${parentName}`;
  const res: Record<string, any> = { [type]: {} };
  fields.forEach(field => {
    res[type][field?.name?.value] = getPresets(field);
  });
  return res;
};

type ArgTypesDefinition =
  | ObjectTypeDefinitionNode
  | InputObjectTypeDefinitionNode;

export const getArgTreeFromPermissionSDL = (
  definition: string,
  introspectionSchema: GraphQLSchema
) => {
  const roots = getSchemaRoots(introspectionSchema);
  try {
    const schema: DocumentNode = parse(definition);
    const defs = schema.definitions as ArgTypesDefinition[];
    const argTree =
      defs &&
      defs.reduce((acc = [], i) => {
        if (i.name && i.fields && roots.includes(i?.name?.value)) {
          const res = getFieldsMap(
            i.fields as FieldDefinitionNode[],
            i.name.value
          );
          return { ...acc, ...res };
        }
        if (i.name && i.fields && i.kind === 'InputObjectTypeDefinition') {
          const type = `input ${i.name.value}`;
          const res: Record<string, any> = { [type]: {} };
          i.fields.forEach(field => {
            if (field.directives && field.directives.length > 0) {
              res[type][field.name?.value] = {};
              res[type][field.name?.value][field.name?.value] = getDirectives(
                field
              );
            }
          });
          return { ...acc, ...res };
        }
        return acc;
      }, {});
    return argTree;
  } catch (e) {
    console.error(e);
    return {};
  }
};

export const generateTypeString = (str: string) => str.replace(/[^\w\s]/gi, '');

// Removes [,],! from params, and returns a new string
export const getTrimmedReturnType = (value: string): string => {
  const typeName = value.replace(/[[\]!]+/g, '');
  return typeName;
};

const getDeps = (field: FieldType, res = new Set<string>([])) => {
  if (field.return) res.add(getTrimmedReturnType(field.return));

  if (field.args)
    Object.values(field.args).forEach(arg => {
      if (!(arg.type instanceof GraphQLScalarType)) {
        const subType = getTrimmedReturnType(arg.type.inspect());
        res.add(subType);
      }
    });

  return res;
};

const addTypesRecursively = (
  list: FieldType[],
  typeList: Set<string>,
  alreadyChecked: Array<string>
): FieldType[] => {
  // if "alreadychecked" has then remove from typelist, if not then add to alreadychecked
  alreadyChecked.forEach(key => {
    if (typeList.has(key)) {
      typeList.delete(key);
    }
  });
  typeList.forEach(value => {
    alreadyChecked.push(value);
  });

  // exit condition
  // if typelist is empty
  if (typeList.size === 0) return list;

  const newList = list.map((fld: FieldType) => {
    const newField = { ...fld };
    if (fld.typeName && typeList.has(fld.typeName)) {
      if (newField.children) {
        const partiallyChecked = newField.children.find(({ checked }) => {
          if (checked) return true;
          return false;
        });
        if (!partiallyChecked)
          newField.children = newField.children.map(ch => {
            if (ch.return) typeList.add(getTrimmedReturnType(ch.return));
            return {
              ...ch,
              checked: true,
            };
          });
      }
    }
    return newField;
  });
  return addTypesRecursively(newList, typeList, alreadyChecked);
};

export const addDepFields = (list: FieldType[], field: FieldType) => {
  const deps = getDeps(field);
  const alreadyChecked: Array<string> = [];
  const newList = addTypesRecursively(list, deps, alreadyChecked);
  return newList;
};

export const getExpandedItems = (list: FieldType[]) => {
  const res: ExpandedItems = {};
  list.forEach((item: FieldType, ix) => {
    const hasValidChildren = item?.children?.find(i => i.checked === true);
    if (!isEmpty(hasValidChildren)) res[ix] = true;
  });
  return res;
};
