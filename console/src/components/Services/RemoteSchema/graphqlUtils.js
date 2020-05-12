import { useEffect, useState } from 'react';
import endpoints from '../../../Endpoints';
import { buildClientSchema, isWrappingType, isObjectType } from 'graphql';

// local cache where introspection schema is cached
let introspectionSchemaCache = {};
export const clearIntrospectionSchemaCache = remoteSchemaName => {
  if (remoteSchemaName) {
    delete introspectionSchemaCache[remoteSchemaName];
  } else {
    introspectionSchemaCache = {};
  }
};

// get graphql introspection proxy endpoint
const getIntrospectionQuery = remoteSchemaName => {
  return {
    type: 'introspect_remote_schema',
    args: {
      name: remoteSchemaName,
    },
  };
};

// custom hook for introspecting remote schema
export const useIntrospectionSchema = (remoteSchemaName, headers) => {
  const [schema, setSchema] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);

  const introspectSchema = () => {
    // if the introspection result is present in cache, skip introspection

    if (!remoteSchemaName) return;

    if (introspectionSchemaCache[remoteSchemaName]) {
      setSchema(introspectionSchemaCache[remoteSchemaName]);
      setLoading(false);
      return;
    }

    // perform introspection
    setLoading(true);
    setError(null);
    fetch(endpoints.query, {
      method: 'POST',
      headers: {
        ...headers,
      },
      body: JSON.stringify(getIntrospectionQuery(remoteSchemaName)),
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

  useEffect(introspectSchema, [remoteSchemaName]);

  return {
    schema,
    loading,
    error,
    introspect: introspectSchema,
  };
};

// get underlying GraphQL type if it is wrapped type
export const getUnderlyingType = t => {
  let currentType = t;
  while (isWrappingType(currentType)) {
    currentType = currentType.ofType;
  }
  return currentType;
};

// get fields of a type from the graphql schema
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
