import {
  GraphQLEnumType,
  GraphQLInputObjectType,
  GraphQLNonNull,
  GraphQLObjectType,
  GraphQLScalarType,
  GraphQLSchema,
} from 'graphql';
import { add } from 'lodash';
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

export const getTree = (schema: any, typeS: any) => {
  const fields =
    typeS === 'QUERY'
      ? schema.getQueryType().getFields()
      : schema.getMutationType().getFields();
  return Object.values(fields).map(
    ({ name, args: argArray, type, ...rest }: any) => {
      const args = argArray.reduce((p, c, cIx) => {
        return { ...p, [c.name]: { ...c } };
      }, {});
      return { name, checked: true, args, return: type.toString(), ...rest };
    }
  );
};

export const getType = (schema: any, scalarTypes: boolean) => {
  const fields = schema.getTypeMap();
  const types: any[] = [];
  Object.entries(fields).forEach(([key, value]: any) => {
    if (
      (!scalarTypes &&
        !(
          value instanceof GraphQLObjectType ||
          value instanceof GraphQLInputObjectType
        )) ||
      (scalarTypes && !(value instanceof GraphQLEnumType))
    )
      return;

    const name = value.inspect();
    if (name === 'query_root' || name === 'mutation_root' || name === 'subscription_root') return;

    if (name.includes('__')) return; // TODO change this check

    const type: any = {};

    if (value instanceof GraphQLObjectType) {
      type.name = `type ${name}`;
    } else if (value instanceof GraphQLInputObjectType) {
      type.name = `input ${name}`;
    } else if (value instanceof GraphQLEnumType) {
      type.name = `enum ${name}`;
    }

    const childArray: any[] = [];
    let fieldVal;

    if (scalarTypes) fieldVal = value.getValues();
    else fieldVal = value.getFields();

    Object.entries(fieldVal).forEach(([k, v]) => {
      if (scalarTypes)
        childArray.push({
          name: v.name,
          checked: true,
          return: v.value.toString(),
        });
      else
        childArray.push({
          name: v.name,
          checked: true,
          return: v.type.toString(),
        });
    });

    type.children = childArray;
    types.push(type);
    // console.log({ types });
  });
  return types;
};

export const getGqlTypeName = typeObj => {
  if (typeObj.ofType) {
    return getGqlTypeName(typeObj.ofType);
  }
  return typeObj.name;
};

const getSDLField = (type, argTree) => {
  let result: any;
  const typeName: string = type.name;
  if (type.name === 'query_root' || type.name === 'mutation_root')
    result = `type ${typeName}{`;
  else result = `${typeName}{`;

  type.children.map(f => {
    // TODO filter selected fields
    if (!f.checked) return null;

    // TODO handle types, this will handle only query and mutations, ie: it adds the brackets
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

export const generateSDL = (types, argTree, schema) => {
  let result = `schema{
    query: query_root
    mutation: mutation_root
}\n`;
  types.forEach(type => {
    result = `${result}\n${getSDLField(type, argTree)}\n`;
  });
  const otherTypes = getType(schema, true);
  otherTypes.forEach(type => {
    result = `${result}\n${getSDLField(type, null)}`;
  });
  result = `${result}\nscalar timestamptz`
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
