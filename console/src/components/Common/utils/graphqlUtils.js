import React from 'react';
import { getIntrospectionQuery, buildClientSchema } from 'graphql';
import endpoints from '../../../Endpoints';

export const useIntrospectionSchema = (headers = {}) => {
  const [schema, setSchema] = React.useState(null);
  const [loading, setLoading] = React.useState(true);
  const [error, setError] = React.useState(null);

  const introspect = () => {
    setLoading(true);
    fetch(endpoints.graphQLUrl, {
      method: 'POST',
      headers,
      body: JSON.stringify({ query: getIntrospectionQuery() }),
    })
      .then(r => r.json())
      .then(response => {
        if (response.data) {
          setSchema(buildClientSchema(response.data));
          setLoading(false);
        } else {
          setLoading(false);
          setError(response);
        }
      })
      .catch(e => {
        setLoading(false);
        setError(e);
      });
    return () => setSchema(null);
  };

  React.useEffect(introspect, []);

  return {
    schema,
    loading,
    error,
    introspect,
  };
};
