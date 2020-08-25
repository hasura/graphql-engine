import React from 'react';
import Tooltip from '../../../../Common/Tooltip/Tooltip';

export const ColumnScalar = ({ argName }) => {
  return (
    <Tooltip
      id="tooltip-remote-rel-scalar-column"
      message={`The value for "${argName}" will be injected from this column value at runtime`}
    />
  );
};

export const RelName = ({ tableName }) => {
  return (
    <Tooltip
      id="tooltip-remote-rel-name"
      message={`The name of the relationship. This will be added as a field under the "${tableName}" node in the GraphQL schema.`}
    />
  );
};

export const RemoteSchema = ({ tableName }) => {
  return (
    <Tooltip
      id="tooltip-remote-rel-remote-schema"
      message={`The remote schema that you wish to join the "${tableName}" table with`}
    />
  );
};

export const Configuration = () => {
  return (
    <Tooltip
      id="tooltip-remote-rel-config"
      message={`Form a GraphQL query and inject the table column values in place of scalar arguments`}
    />
  );
};
