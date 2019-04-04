import React from 'react';

/* This function sets the styling to the way the relationship looks, for eg: article.id -> user.user_id */
export const getRelDef = (isObjRel, _lCol, _rCol, lTable, _rTable) => {
  const rTable = _rTable.name ? _rTable.name : _rTable;

  const lCol = _lCol.length > 1 ? '( ' + _lCol.join(', ') + ' )' : _lCol[0];
  const rCol = _rCol.length > 1 ? '( ' + _rCol.join(', ') + ' )' : _rCol[0];

  return isObjRel ? (
    <span>
      {lTable} . {lCol} &nbsp;&rarr;&nbsp;
      {rTable} . {rCol}
    </span>
  ) : (
    <span>
      {rTable} . {rCol} &nbsp;&rarr;&nbsp;
      {lTable} . {lCol}
    </span>
  );
};
