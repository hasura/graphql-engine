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

/* Gets the complete list of relationships and converts it to a list of object, which looks like so :
[ { objRel: {objectRelationship}, arrRel: {arrayRelationship} } ] */
export const getObjArrRelList = relationships => {
  const objRels = relationships.filter(r => r.rel_type === 'object');
  const arrRels = relationships.filter(r => r.rel_type !== 'object');

  const requiredList = [];
  const length =
    objRels.length > arrRels.length ? objRels.length : arrRels.length;

  for (let i = 0; i < length; i++) {
    const objRel = objRels[i] ? objRels[i] : null;
    const arrRel = arrRels[i] ? arrRels[i] : null;

    requiredList.push({
      objRel,
      arrRel,
    });
  }

  return requiredList;
};
