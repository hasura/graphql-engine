import {
  GraphQLEnumType,
  GraphQLInputObjectType,
  GraphQLNonNull,
  GraphQLObjectType,
  GraphQLScalarType,
  GraphQLSchema,
} from 'graphql';
// import { add, result } from 'lodash';
import { findRemoteSchemaPermission } from '../utils';
import { PermissionEdit, SchemaDefinition } from './types';

export const getCreateRemoteSchemaPermissionQuery = (
  def: { role: string },
  remoteSchemaName: string,
  // schemaDefinition: SchemaDefinition
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
  // schemaDefinition: SchemaDefinition
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
      ? introspectionSchema!.getQueryType()!.getFields()
      : introspectionSchema!.getMutationType()!.getFields();

  let permissionsSchemaFields: any = null; // TODO use ternary operator
  if (permissionsSchema !== null) {
    permissionsSchemaFields =
      typeS === 'QUERY'
        ? permissionsSchema!.getQueryType()!.getFields()
        : permissionsSchema!.getMutationType()!.getFields();
  }

  return Object.values(introspectionSchemaFields).map(
    ({ name, args: argArray, type, ...rest }: any) => {
      let checked = true;
      const args = argArray.reduce((p, c, cIx) => {
        return { ...p, [c.name]: { ...c } };
      }, {});
      if (permissionsSchema !== null && !(name in permissionsSchemaFields)) {
        checked = false;
      }
      return { name, checked, args, return: type.toString(), ...rest };
    }
  );
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

    const type: any = {};

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
  Object.entries(fields).forEach(([key, value]: any) => {
    if (!(value instanceof GraphQLEnumType)) return;
    const name = value.inspect();

    if (name.includes('__')) return; // TODO change this check

    const type: any = {};
    type.name = `enum ${name}`;

    const childArray: any[] = [];
    const fieldVal = value.getValues();

    Object.entries(fieldVal).forEach(([k, v]) => {
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
  Object.entries(fields).forEach(([key, value]: any) => {
    if (!(value instanceof GraphQLScalarType)) return;
    const name = value.inspect();
    if (gqlDefaultTypes.indexOf(name) > -1) return; // Check if type belongs to default gql scalar types

    const type = `scalar ${name}`;
    types.push(type);
  });
  return types;
};

export const getGqlTypeName = typeObj => {
  if (typeObj.ofType) {
    return getGqlTypeName(typeObj.ofType);
  }
  return typeObj.name;
};

const checkNullType = type => {
  const isChecked = element => element.checked;
  return type.children.some(isChecked);
};

const getSDLField = (type, argTree) => {
  if (!checkNullType(type)) return '';

  let result: any;
  const typeName: string = type.name;
  if (type.name === 'query_root' || type.name === 'mutation_root')
    result = `type ${typeName}{`;
  else result = `${typeName}{`;

  type.children.map(f => {
    // TODO filter selected fields
    if (!f.checked) return null;

    let fieldStr = f.name;

    if (!typeName.includes('enum')) {
      if (f?.args) {
        fieldStr = `${fieldStr}(`;
        Object.values(f.args).map((arg: any) => {
          let valueStr = ``;
          if (argTree && argTree[f.name] && argTree[f.name][arg.name]) {
            const jsonStr = JSON.stringify(argTree[f.name][arg.name]);
            const unquoted = jsonStr.replace(/"([^"]+)":/g, '$1:');
            valueStr = `${arg.name} : ${getGqlTypeName(
              arg.type
            )} @preset(value: ${unquoted})`;
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

export const generateSDL = (types, argTree) => {
  //   let result = `schema{
  //       query: query_root
  //       mutation: mutation_root
  //   }\n
  //   scalar PresetValue
  //   \n
  //   directive @preset(
  //       value: PresetValue
  //   ) on INPUT_FIELD_DEFINITION | ARGUMENT_DEFINITION\n
  // `;
  let result = `schema{
  query: query_root
  mutation: mutation_root
}\n`;
  types.forEach(type => {
    result = `${result}\n${getSDLField(type, argTree)}\n`;
  });

  return result;
};

export const generateConstantTypes = (schema: GraphQLSchema) => {
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

export const getChildArgument = v => {
  if (typeof v === 'string') return { children: null }; // value field
  if (v?.type instanceof GraphQLInputObjectType && v?.type?.getFields)
    return { children: v?.type?.getFields(), path: 'type._fields' };
  if (v?.type instanceof GraphQLNonNull || v?.type?.ofType) {
    return { children: v?.type?.ofType?._fields, path: 'type.ofType._fields' };
  }
  return {};
};
