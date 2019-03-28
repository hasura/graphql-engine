import React from 'react';

/* This function sets the styling to the way the relationship looks, for eg: id -> user::user_id */
export const getRelDef = (isObjRel, lcol, rcol, rTable) => {
  const finalRTable = rTable.name ? rTable.name : rTable;
  return isObjRel ? (
    <span>
      {lcol.join(',')}
      &nbsp;&rarr;&nbsp;
      {rTable} :: {rcol.join(',')}
    </span>
  ) : (
    <span>
      {finalRTable} :: {rcol.join(',')}
      &nbsp;&rarr;&nbsp;
      {lcol.join(',')}
    </span>
  );
};
