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
    A primary-key is a column (or set of columns) that uniquely identifies a row
    of a table
  </Tooltip>
);

export const foreignKeyDescription = (
  <Tooltip id="tooltip-foreign-key-description">
    A foreign-key is a column (or set of columns) in one table that uniquely
    identifies a row of another table (can be the same table). They are used to
    establish a link between the data in the two tables.
  </Tooltip>
);

export const uniqueKeyDescription = (
  <Tooltip id="tooltip-foreign-key-description">
    A unique key is a column (or set of columns) in a table that has unique
    values across the table.
  </Tooltip>
);

export const dataTypeDescription = description => (
  <Tooltip id="tooltip-datatype-description">{description}</Tooltip>
);
