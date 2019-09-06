import { permissionState } from '../state';
import { getUnderlyingType } from '../graphqlUtils';
import { isObjectType } from 'graphql';
import { getTypeFields } from '../graphqlUtils';

export const generateDropPermQuery = (role, remoteSchemaName) => {
  return {
    type: 'drop_remote_schema_permissions',
    args: {
      remote_schema: remoteSchemaName,
      role,
    },
  };
};

export const generateCreatePermQuery = (state, remoteSchemaName) => {
  const { role, allowedTypes, isNew, newRole } = state;

  const bulkQueryArgs = [];

  if (!isNew) {
    bulkQueryArgs.push(generateDropPermQuery(role, remoteSchemaName));
  }
  const payload = {
    type: 'add_remote_schema_permissions',
    args: {
      remote_schema: remoteSchemaName,
      role: newRole || role,
      definition: [],
    },
  };

  Object.keys(allowedTypes).forEach(allowedType => {
    payload.args.definition[allowedType] = Object.keys(
      allowedTypes[allowedType]
    ).filter(fieldName => allowedTypes[allowedType][fieldName].isChecked);
    payload.args.definition.push({
      type: allowedType,
      fields: Object.keys(allowedTypes[allowedType]).filter(
        fieldName => allowedTypes[allowedType][fieldName].isChecked
      ),
    });
  });

  bulkQueryArgs.push(payload);

  return {
    type: 'bulk',
    args: bulkQueryArgs,
  };
};

export const parseRemoteRelPermDefinition = (
  payload,
  rootTypes,
  objectTypes,
  nonObjectTypes,
  roleName
) => {
  if (!payload) {
    const newAllowedTypes = {};
    Object.keys(rootTypes).forEach(rt => {
      newAllowedTypes[rootTypes[rt]] = getTypeFields(
        rootTypes[rt],
        objectTypes,
        nonObjectTypes
      );
    });
    const parsedDef = {
      ...permissionState.editState,
      allowedTypes: newAllowedTypes,
      isNew: true,
    };
    if (payload === null) {
      parsedDef.newRole = roleName;
    } else {
      parsedDef.role = roleName;
    }
    return parsedDef;
  }

  const { definition, role } = payload;

  const allowedTypes = {};

  definition.forEach(allowedType => {
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
    isNew: false,
  };
};

export const getExpandedTypes = (allowedTypes, rootTypes, editType) => {
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
