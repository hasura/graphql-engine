import { getUnderlyingType } from '../graphqlUtils';
import { isObjectType } from 'graphql';

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

export const parseRemoteRelPermDefinition = (payload, objectTypes) => {
  const { definition, role } = payload;
  const allowedTypes = {};

  Object.keys(definition).forEach(allowedType => {

    const fieldMetaData = {};
    const selectedFields = {};

    definition[allowedType].forEach(selectedFeldName => {
      selectedFields[selectedFeldName] = true
    });

    console.log('=================================');
    console.log(objectTypes);
    console.log(allowedType)
    console.log(objectTypes[allowedType])
    console.log('=================================');

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
