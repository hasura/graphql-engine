import { getTableName } from '../utils';

const suggestedRelationshipsRaw = (tableName, allSchemas) => {
  const objRels = [];
  const arrRels = [];
  const currentTableSchema = allSchemas.find(t => t.table_name === tableName);
  const currentTableRelationships = currentTableSchema.relationships;
  const currentObjRels = currentTableRelationships.filter(
    r => r.rel_type === 'object'
  );
  const currentArrRels = currentTableRelationships.filter(
    r => r.rel_type === 'array'
  );
  // Object Relationships
  currentTableSchema.foreign_key_constraints.forEach(fk_obj => {
    if (!fk_obj.is_ref_table_tracked) {
      return;
    }
    const lcol = Object.keys(fk_obj.column_mapping);
    let isExistingObjRel = false;
    for (let k = 0; k < currentObjRels.length; k++) {
      if (currentObjRels[k].rel_def.foreign_key_constraint_on) {
        // check if this is already an existing relationship
        if (
          // TODO: ideally multiple columns should be handled by server
          lcol.length === 1 &&
          currentObjRels[k].rel_def.foreign_key_constraint_on === lcol[0]
        ) {
          // existing relationship
          isExistingObjRel = true;
        }
      } else {
        // check if this is already an existing relationship
        if (
          Object.keys(
            currentObjRels[k].rel_def.manual_configuration.column_mapping
          )
            .sort()
            .join(',') === lcol.sort().join(',')
        ) {
          // existing relationship
          isExistingObjRel = true;
        }
      }
    }
    if (!isExistingObjRel) {
      objRels.push({
        tableName: tableName,
        isObjRel: true,
        name: null,
        lcol: lcol,
        rcol: lcol.map(column => fk_obj.column_mapping[column]),
        rTable: fk_obj.ref_table,
      });
    }
  });
  // Array relationships
  currentTableSchema.opp_foreign_key_constraints.forEach(o_fk_obj => {
    if (!o_fk_obj.is_table_tracked) {
      return;
    }
    const rcol = Object.keys(o_fk_obj.column_mapping);
    const rTable = o_fk_obj.table_name;
    let isExistingArrayRel = false;
    for (let k = 0; k < currentArrRels.length; k++) {
      // check if this is already an existing relationship
      const relDef = currentArrRels[k].rel_def;
      let currTable = null;
      let currRCol = null;

      if (relDef.foreign_key_constraint_on) {
        currTable = relDef.foreign_key_constraint_on.table;
        currRCol = [relDef.foreign_key_constraint_on.column];
      } else {
        currTable = relDef.manual_configuration.remote_table;
        currRCol = Object.values(relDef.manual_configuration.column_mapping);
      }
      if (
        currRCol.sort().join(',') === rcol.sort().join(',') &&
        getTableName(currTable) === o_fk_obj.table_name
      ) {
        // existing relationship
        isExistingArrayRel = true;
      }
    }
    if (!isExistingArrayRel) {
      arrRels.push({
        tableName: tableName,
        isObjRel: false,
        name: null,
        rcol: rcol,
        lcol: rcol.map(column => o_fk_obj.column_mapping[column]),
        rTable: rTable,
      });
    }
  });

  const length =
    objRels.length > arrRels.length ? objRels.length : arrRels.length;
  const finalObjRel = [];
  const finalArrayRel = [];
  for (let i = 0; i < length; i++) {
    const objRel = objRels[i] ? objRels[i] : null;
    const arrRel = arrRels[i] ? arrRels[i] : null;
    if (objRel !== null) {
      finalObjRel.push(objRel);
    }
    if (arrRel !== null) {
      finalArrayRel.push(arrRel);
    }
  }

  return { objectRel: finalObjRel, arrayRel: finalArrayRel };
};

export default suggestedRelationshipsRaw;
