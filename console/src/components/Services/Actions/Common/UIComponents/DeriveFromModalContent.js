import React from 'react';
import Spinner from '../../../../Common/Spinner/Spinner';
import endpoints from '../../../../../Endpoints';
import { connect } from 'react-redux';
import {
  getIntrospectionQuery,
  buildClientSchema,
  isInputObjectType,
} from 'graphql';

const useIntrospectionSchema = headers => {
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
          setError(e);
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

const DeriveFrom = ({ headers }) => {
  const { schema, loading, error, introspect } = useIntrospectionSchema(
    headers
  );

  if (loading) return <Spinner />;

  if (error) {
    return (
      <div>
        Error introspecting schema.&nbsp;
        <a onClick={introspect}>Try again</a>
      </div>
    );
  }

  const inputObjectTypes = Object.keys(schema._typeMap).filter(t => {
    return isInputObjectType(schema._typeMap[t]);
  });

  const onSelection = e => {
    const selectedType = e.target.value;
    if (selectedType === '') return;
    console.log(selectedType);
  };

  return (
    <div>
      <select value="" className={'form-control'} onChange={onSelection}>
        {inputObjectTypes.map(t => {
          return <option value={t}>{t}</option>;
        })}
      </select>
    </div>
  );
};

const mapStateToprops = state => {
  return {
    headers: state.tables.dataHeaders,
  };
};

export default connect(mapStateToprops)(DeriveFrom);
