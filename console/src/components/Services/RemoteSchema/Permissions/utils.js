import globals from '../../../../Globals';
import { getUnderlyingType } from '../graphqlUtils';
import { isObjectType } from 'graphql';

// generate the query to drop a remote schema permission
export const generateDropPermQuery = (role, remoteSchemaName) => {
  return {
    type: 'drop_remote_schema_permissions',
    args: {
      remote_schema: remoteSchemaName,
      role,
    },
  };
};

// get all object types with their fields
export const getAllObjectTypes = objectTypes => {
  return Object.values(objectTypes).map(t => {
    return {
      type: t.name,
      fields: Object.keys(t._fields),
    };
  });
};

// generate the query to create a remote schema permission
export const generateCreatePermQuery = (
  state,
  remoteSchemaName,
  isExisting
) => {
  const { role, allowedTypes, newRole } = state;

  if (isExisting) {
    return {
      type: 'add_remote_schema_permissions',
      args: {
        ...state,
      },
    };
  }

  const payload = {
    type: 'add_remote_schema_permissions',
    args: {
      remote_schema: remoteSchemaName,
      role: newRole || role,
      definition: {
        allowed_objects: [],
      },
    },
  };

  Object.keys(allowedTypes).forEach(allowedType => {
    payload.args.definition.allowed_objects.push({
      type: allowedType,
      fields: Object.keys(allowedTypes[allowedType]).filter(
        fieldName => allowedTypes[allowedType][fieldName].isChecked
      ),
    });
  });

  return payload;
};

// transform server's remote schema perm definition into the local state definition
export const parseRemoteRelPermDefinition = (
  payload,
  rootTypes,
  objectTypes,
  nonObjectTypes,
  roleName,
  isNew
) => {
  if (!payload) {
    const mockPayload = {
      role: roleName,
      definition: {
        allowed_objects: getAllObjectTypes(objectTypes),
      },
    };
    return parseRemoteRelPermDefinition(
      mockPayload,
      rootTypes,
      objectTypes,
      nonObjectTypes,
      roleName,
      true
    );
  }

  const { definition, role } = payload;

  const allowedTypes = {};

  definition.allowed_objects.forEach(allowedType => {
    const allowedTypeName = allowedType.type;
    const fieldMetaData = {};
    const selectedFields = {};

    allowedType.fields.forEach(selectedFeldName => {
      selectedFields[selectedFeldName] = true;
    });

    const graphqlType = getUnderlyingType(objectTypes[allowedTypeName]);
    Object.keys(graphqlType._fields).forEach(field => {
      const returningType = getUnderlyingType(graphqlType._fields[field].type);
      fieldMetaData[field] = {
        isChecked: !!selectedFields[field],
        typeName: returningType.name,
        isScalar: !isObjectType(returningType),
      };
    });
    allowedTypes[allowedTypeName] = fieldMetaData;
  });

  return {
    role,
    allowedTypes,
    isNew: !!isNew,
  };
};

// get the current expanded types based on the allowed types and the type of selected role and quer type
export const getExpandedTypes = (
  allowedTypes,
  rootTypes,
  editType,
  allowNesting
) => {
  if (!allowNesting) {
    return {
      [rootTypes[editType]]: true,
    };
  }

  const expandedTypes = {};

  const expandTypes = currentTypeName => {
    expandedTypes[currentTypeName] = true;
    Object.keys(allowedTypes[currentTypeName]).forEach(fieldName => {
      const allowedType = allowedTypes[currentTypeName][fieldName];
      if (allowedType.isChecked && !allowedType.isScalar) {
        if (!expandedTypes[allowedTypes[currentTypeName][fieldName].typeName]) {
          expandTypes(allowedTypes[currentTypeName][fieldName].typeName);
        }
      }
    });
  };

  expandTypes(rootTypes[editType]);

  return expandedTypes;
};

// looking at the state and a given typename, figure out if there is full access to this type
export const isTypeFullAccess = (
  typeName,
  allowedTypes,
  objectTypes,
  cache
) => {
  cache[typeName] = true;
  if (!allowedTypes[typeName]) return false;

  const currentTypeSelected = allowedTypes[typeName];
  const currentTypeOriginal = objectTypes[typeName];

  const allTypeFields = Object.keys(currentTypeOriginal._fields);

  for (let i = allTypeFields.length - 1; i >= 0; i--) {
    const currentField = currentTypeSelected[allTypeFields[i]];

    if (!currentField) {
      return false;
    }

    if (!currentField.isChecked) {
      return false;
    }

    if (!currentField.isScalar) {
      if (!cache[currentField.typeName]) {
        const typeAccess = isTypeFullAccess(
          currentField.typeName,
          allowedTypes,
          objectTypes,
          cache
        );
        if (!typeAccess) {
          return false;
        }
      }
    }
  }

  return true;
};

// get full/partial/no access to a root type
export const getRootTypeAccess = (rootType, allowedTypes, objectTypes) => {
  const accessLabels = {
    '-1': 'noAccess',
    0: 'partialAccess',
    1: 'fullAccess',
  };

  let access = -1;

  if (allowedTypes[rootType]) {
    access = 0;
  }

  let traversedTypesCache = {};
  if (
    isTypeFullAccess(rootType, allowedTypes, objectTypes, traversedTypesCache)
  ) {
    access = 1;
  }

  traversedTypesCache = {};
  return accessLabels[access];
};

export const isRemoteSchemaPermissionsEnabled = () => {
  return !!globals.enableRemoteSchemaPermissions;
};
