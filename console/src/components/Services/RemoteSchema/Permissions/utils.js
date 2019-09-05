import { permissionState } from '../state';
import { getUnderlyingType } from '../graphqlUtils';
import { isObjectType } from 'graphql';
import { getTypeFields } from '../graphqlUtils';

export const generateDropPermQuery = (role, remoteSchemaName) => {
  return {
    type: 'drop_remote_schema_permissions',
    args: {
      remote_schema: remoteSchemaName,
      role
    }
  };
};

export const generateCreatePermQuery = (state, remoteSchemaName) => {
  const { role, allowedTypes, isNew } = state;

  const bulkQueryArgs = [];

  if (!isNew) {
    bulkQueryArgs.push(generateDropPermQuery(role, remoteSchemaName));
  }
  const payload = {
    type: 'add_remote_schema_permissions',
    args: {
      remote_schema: remoteSchemaName,
      role,
      definition: [],
    },
  };

  Object.keys(allowedTypes).forEach(allowedType => {
    payload.args.definition[allowedType] = Object.keys(
      allowedTypes[allowedType]
    ).filter(fieldName => allowedTypes[allowedType][fieldName].isChecked);
    payload.args.definition.push({
      type: allowedType,
      fields: Object.keys(
        allowedTypes[allowedType]
      ).filter(fieldName => allowedTypes[allowedType][fieldName].isChecked)
    });
  });

  bulkQueryArgs.push(payload);

  return {
    type: 'bulk',
    args: bulkQueryArgs
  };
};

export const parseRemoteRelPermDefinition = (payload, rootTypes, objectTypes, nonObjectTypes, roleName) => {
  if (!payload) {
    const newAllowedTypes = {};
    Object.keys(rootTypes).forEach(rt => {
      newAllowedTypes[rootTypes[rt]] = getTypeFields(rootTypes[rt], objectTypes, nonObjectTypes);
    });
    return {
      ...permissionState.editState,
      allowedTypes: newAllowedTypes,
      role: roleName,
      isNew: true,
    };
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
        isScalar: !isObjectType(returningType)
      };
    });
    allowedTypes[allowedTypeName] = fieldMetaData;
  });

  return {
    role,
    allowedTypes,
    isNew: false
  };
};

export const getExpandedTypes = (allowedTypes, rootTypes, editType) => {
  const expandedTypes = {};

  const expandTypes = (currentTypeName) => {
    expandedTypes[currentTypeName] = true;
    Object.keys(allowedTypes[currentTypeName]).forEach(fieldName => {
      const allowedType = allowedTypes[currentTypeName][fieldName];
      if (allowedType.isChecked && !allowedType.isScalar) {
        expandTypes(allowedTypes[currentTypeName][fieldName].typeName);
      }
    });
  };

  expandTypes(rootTypes[editType]);

  return expandedTypes;
};
