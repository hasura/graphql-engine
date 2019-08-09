import React from 'react';
import { isObjectType } from 'graphql';
import { useIntrospectionSchema } from '../graphqlUtils';
import PermissionsEditor from './PermissionsEditor';

const Permissions = props => {
  const {
    permissions: { rolePermissions },
    remoteSchemaName,
    adminHeaders,
    remoteSchemasList,
  } = props;
  const currentRemoteSchema = remoteSchemasList.find(
    r => r.name === remoteSchemaName
  );
  const { schema, loading, error, introspect } = useIntrospectionSchema(
    currentRemoteSchema.definition.url,
    adminHeaders
  );

  if (loading) return 'Loading...';
  if (error) {
    return (
      <div>
        <p>
          Error. <a onClick={introspect}>Retry </a>
        </p>
      </div>
    );
  }

  const rootTypes = { query: '', mutation: '', subscription: '' };
  const queryTypeName = schema._queryType.name;
  rootTypes.query = queryTypeName;

  const mutationTypeName = schema._mutationType
    ? schema._mutationType.name
    : '';
  rootTypes.mutation = mutationTypeName;

  const subscriptionTypeName = schema._subscriptionType
    ? schema._subscriptionType.name
    : '';
  rootTypes.subscription = subscriptionTypeName;

  const objectTypes = {};
  const nonObjectTypes = {};
  objectTypes[queryTypeName] = schema._typeMap[queryTypeName];
  if (mutationTypeName) {
    objectTypes[mutationTypeName] = schema._typeMap[mutationTypeName];
  }
  Object.keys(schema._typeMap)
    .sort()
    .forEach(t => {
      if (
        t !== queryTypeName &&
        t !== mutationTypeName &&
        t.indexOf('__') !== 0
      ) {
        const currentType = schema._typeMap[t];
        if (isObjectType(currentType)) {
          objectTypes[t] = currentType;
        } else {
          nonObjectTypes[t] = currentType;
        }
      }
    });

  const numPermissions = rolePermissions.length;
  return rolePermissions.map((rp, index) => {
    return (
      <PermissionsEditor
        permission={rp}
        objectTypes={objectTypes}
        nonObjectTypes={nonObjectTypes}
        key={index}
        index={index}
        numPermissions={numPermissions}
        isLast={index === numPermissions - 1}
        rootTypes={rootTypes}
        dispatch={props.dispatch}
      />
    );
  });
};

export default Permissions;
