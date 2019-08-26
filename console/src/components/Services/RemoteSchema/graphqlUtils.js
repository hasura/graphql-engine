import { useEffect, useState } from 'react';
import {
  buildClientSchema,
  getIntrospectionQuery,
  isWrappingType,
  isObjectType,
} from 'graphql';

export const useIntrospectionSchema = (endpoint, headers) => {
  const [schema, setSchema] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);

  const introspectSchema = () => {
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
        setSchema(buildClientSchema(response.data));
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
