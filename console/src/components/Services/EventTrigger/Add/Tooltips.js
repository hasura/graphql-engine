import React from 'react';
import Tooltip from 'react-bootstrap/lib/Tooltip';

export const databaseNamingScheme = (
  <Tooltip id="tooltip-database-naming-scheme">
    Recommend using snake_case naming convertion for table and columns. eg:
    product_skus
  </Tooltip>
);

export const primaryKeyDescription = (
  <Tooltip id="tooltip-primary-key-description">
    The 'Primary Key' is a unique, non-null constraint which is used to uniquely
    identifies a record/row on a table. A primary key can be a single column or
    compose of multiple columns.
  </Tooltip>
);

export const dataTypeDescription = description => (
  <Tooltip id="tooltip-datatype-description">{description}</Tooltip>
);
