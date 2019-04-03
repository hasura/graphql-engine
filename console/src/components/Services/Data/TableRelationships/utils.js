import React from 'react';

/* This function sets the styling to the way the relationship looks, for eg: article.id -> user.user_id */
export const getRelDef = (isObjRel, lcol, rcol, lTable, _rTable) => {
  const rTable = _rTable.name ? _rTable.name : _rTable;

  return isObjRel ? (
    <span>
      {lTable} . {lcol.join(',')}
      &nbsp;&rarr;&nbsp;
      {_rTable} . {rcol.join(',')}
    </span>
  ) : (
    <span>
      {rTable} . {rcol.join(',')}
      &nbsp;&rarr;&nbsp;
      {lTable} . {lcol.join(',')}
    </span>
  );
};
