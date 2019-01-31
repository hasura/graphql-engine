import React from 'react';

/* This function sets the styling to the way the relationship looks, for eg: id -> user::user_id */
export const getRelationshipLine = (isObjRel, lcol, rcol, rTable) => {
  const finalRTable = rTable.name ? rTable.name : rTable;
  return isObjRel ? (
    <span>
      &nbsp;
      {lcol.join(',')}
      &nbsp;&nbsp;&rarr;&nbsp;&nbsp;
      {rTable} :: {rcol.join(',')}
    </span>
  ) : (
    <span>
      &nbsp;
      {finalRTable} :: {rcol.join(',')}
      &nbsp;&nbsp;&rarr;&nbsp;&nbsp;
      {lcol.join(',')}
    </span>
  );
};
