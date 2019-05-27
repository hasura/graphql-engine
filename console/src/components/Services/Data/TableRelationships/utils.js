import React from 'react';

/* This function sets the styling to the way the relationship looks, for eg: article.id -> user.user_id */
export const getRelDef = relMeta => {
  const lcol =
    relMeta.lcol.length > 1
      ? '( ' + relMeta.lcol.join(', ') + ' )'
      : relMeta.lcol[0];
  const rcol =
    relMeta.rcol.length > 1
      ? '( ' + relMeta.rcol.join(', ') + ' )'
      : relMeta.rcol[0];

  return relMeta.isObjRel ? (
    <span>
      {relMeta.lTable} . {lcol} &nbsp;&rarr;&nbsp;
      {relMeta.rTable} . {rcol}
    </span>
  ) : (
    <span>
      {relMeta.rTable} . {rcol} &nbsp;&rarr;&nbsp;
      {relMeta.lTable} . {lcol}
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
