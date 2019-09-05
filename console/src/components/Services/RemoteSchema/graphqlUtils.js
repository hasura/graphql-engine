import { useEffect, useState } from 'react';
import {
  buildClientSchema,
  getIntrospectionQuery,
  isWrappingType,
  isObjectType,
} from 'graphql';

let introspectionSchemaCache = {};
export const clearIntrospectionSchemaCache = (remoteSchemaName) => {
  if (remoteSchemaName) {
    delete introspectionSchemaCache[remoteSchemaName];
  } else {
    introspectionSchemaCache = {};
  }
};

export const useIntrospectionSchema = (endpoint, headers, remoteSchemaName) => {
  const [schema, setSchema] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);

  const introspectSchema = () => {
    if (introspectionSchemaCache[remoteSchemaName]) {
      setSchema(introspectionSchemaCache[remoteSchemaName]);
      setLoading(false);
      return;
    }
    setLoading(true);
    setError(null);
    fetch(endpoint, {
      method: 'POST',
      headers: {
        ...headers,
      },
      body: JSON.stringify({ query: getIntrospectionQuery() }),
    })
      .then(r => r.json())
      .then(response => {
        const clientSchema = buildClientSchema(response.data);
        setSchema(clientSchema);
        introspectionSchemaCache[remoteSchemaName] = clientSchema;
        setLoading(false);
      })
      .catch(err => {
        console.error(err);
        setError(err);
        setLoading(false);
      });
  };

  useEffect(introspectSchema, []);

  return {
    schema,
    loading,
    error,
    introspect: introspectSchema,
  };
};

export const getUnderlyingType = t => {
  let currentType = t;
  while (isWrappingType(currentType)) {
    currentType = currentType.ofType;
  }
  return currentType;
};

export const getTypeFields = (typeName, objectTypes) => {
  const fields = {};
  if (objectTypes[typeName]) {
    const type = getUnderlyingType(objectTypes[typeName]);
    Object.keys(type._fields).forEach(field => {
      const fieldType = getUnderlyingType(type._fields[field].type);
      fields[field] = {
        typeName: fieldType.name,
        isScalar: !isObjectType(fieldType),
        isChecked: false,
      };
    });
  }
  return fields;
};
