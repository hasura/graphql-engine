import { permissionState } from '../state';
import { getUnderlyingType } from '../graphqlUtils';
import { isObjectType } from 'graphql';
import { getTypeFields } from '../graphqlUtils';

export const generateCreatePermQuery = (state, remoteSchemaName) => {

  const { role, allowedTypes } = state;

  const payload = {
    type: 'add_remote_schema_permissions',
    args: {
      remote_schema: remoteSchemaName,
      role,
      definition: {},
    },
  };

  Object.keys(allowedTypes).forEach(allowedType => {
    payload.args.definition[allowedType] = Object.keys(
      allowedTypes[allowedType]
    ).filter(fieldName => allowedTypes[allowedType][fieldName].isChecked);
  });

  return payload;

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
      role: roleName
    };
  }

  const { definition, role } = payload;

  const allowedTypes = {};

  Object.keys(definition).forEach(allowedType => {

    const fieldMetaData = {};
    const selectedFields = {};

    definition[allowedType].forEach(selectedFeldName => {
      selectedFields[selectedFeldName] = true
    });

    const graphqlType = getUnderlyingType(objectTypes[allowedType]);
    Object.keys(graphqlType._fields).forEach(field => {
      const returningType = getUnderlyingType(graphqlType._fields[field].type);
      fieldMetaData[field] = {
        isChecked: !!selectedFields[field],
        typeName: returningType.name,
        isScalar: !isObjectType(returningType)
      }
    })
    allowedTypes[allowedType] = fieldMetaData;
  })

  return {
    role,
    allowedTypes
  };

};


export const getExpandedTypes = (allowedTypes, rootTypes, editType) => {

  const expandedTypes = {};

  const expandTypes = (currentTypeName) => {
    console.log(currentTypeName);
    expandedTypes[currentTypeName] = true;
    Object.keys(allowedTypes[currentTypeName]).forEach(fieldName => {
      const allowedType = allowedTypes[currentTypeName][fieldName];
      if (allowedType.isChecked && !allowedType.isScalar) {
        expandTypes(allowedTypes[currentTypeName][fieldName].typeName)
      }
    });
  };

  expandTypes(rootTypes[editType]);

  return expandedTypes;
};