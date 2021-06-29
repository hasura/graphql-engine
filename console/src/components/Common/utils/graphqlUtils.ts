import {
  buildClientSchema,
  getIntrospectionQuery,
  GraphQLSchema,
} from 'graphql';
import React from 'react';
import endpoints from '../../../Endpoints';
import { Dispatch } from '../../../types';
import requestAction from '../../../utils/requestAction';

export const getGraphQLQueryPayload = (
  query: string,
  variables: Record<string, any>
) => ({
  query,
  variables,
});

export const useIntrospectionSchema = (headers = {}, dispatch: Dispatch) => {
  const [schema, setSchema] = React.useState<GraphQLSchema | null>(null);
  const [loading, setLoading] = React.useState(true);
  const [error, setError] = React.useState(null);

  const introspect = () => {
    setLoading(true);

    dispatch(
      requestAction(
        endpoints.graphQLUrl,
        {
          method: 'POST',
          headers,
          body: JSON.stringify(
            getGraphQLQueryPayload(getIntrospectionQuery(), {})
          ),
        },
        undefined,
        undefined,
        true,
        true
      )
    )
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

  React.useEffect(introspect, [dispatch, headers]);

  return {
    schema,
    loading,
    error,
    introspect,
  };
};
