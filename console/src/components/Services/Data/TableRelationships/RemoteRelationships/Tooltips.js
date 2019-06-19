import React from 'react';
import Tooltip from 'react-bootstrap/lib/Tooltip';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import styles from './SchemaExplorer.scss';

const getOverlayTrigger = tooltip => {
  return (
    <OverlayTrigger placement="right" overlay={tooltip}>
      <i
        className={`fa fa-question-circle ${styles.tooltip}`}
        aria-hidden="true"
      />
    </OverlayTrigger>
  );
};

export const columnScalar = argName => {
  const tooltip = (
    <Tooltip id="tooltip-remote-rel-scalar-column">
      The value for <b>{argName}</b> will be injected from this column value at
      runtime
    </Tooltip>
  );
  return getOverlayTrigger(tooltip);
};

export const relName = tableName => {
  const tooltip = (
    <Tooltip id="tooltip-remote-rel-name">
      The name of the relationship. This will be added as a field under the{' '}
      <b>{tableName}</b> node in the GraphQL schema.
    </Tooltip>
  );
  return getOverlayTrigger(tooltip);
};

export const remoteSchema = tableName => {
  const tooltip = (
    <Tooltip id="tooltip-remote-rel-remote-schema">
      The remote schema that you wish to join the <b>{tableName}</b> table with
    </Tooltip>
  );
  return getOverlayTrigger(tooltip);
};

export const configuration = () => {
  const tooltip = (
    <Tooltip id="tooltip-remote-rel-config">
      Form a GraphQL query and inject the table column values in place of scalar
      arguments
    </Tooltip>
  );
  return getOverlayTrigger(tooltip);
};
