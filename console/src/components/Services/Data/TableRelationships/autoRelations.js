import { getTableName } from '../utils';

const sameRelCols = (currCols, existingCols) => {
  return currCols.sort().join(',') === existingCols.sort().join(',');
};

const isExistingObjRel = (currentObjRels, relCols) => {
  let _isExistingObjRel = false;

  for (let k = 0; k < currentObjRels.length; k++) {
    const objRelDef = currentObjRels[k].rel_def;

    if (objRelDef.foreign_key_constraint_on) {
      // check if this is already an existing fk relationship
      if (
        // TODO: update when multiCol fkey rels are allowed
        relCols.length === 1 &&
        objRelDef.foreign_key_constraint_on === relCols[0]
      ) {
        _isExistingObjRel = true;
        break;
      }
    } else {
      // check if this is already an existing manual relationship
      const objRelCols = Object.keys(
        objRelDef.manual_configuration.column_mapping
      );
      if (sameRelCols(objRelCols, relCols)) {
        _isExistingObjRel = true;
        break;
      }
    }
  }

  return _isExistingObjRel;
};

const isExistingArrRel = (currentArrRels, relCols, relTable) => {
  let _isExistingArrRel = false;

  for (let k = 0; k < currentArrRels.length; k++) {
    const arrRelDef = currentArrRels[k].rel_def;

    let currTable = null;
    let currRCol = null;

    if (arrRelDef.foreign_key_constraint_on) {
      // check if this is already an existing fk relationship
      currTable = arrRelDef.foreign_key_constraint_on.table;
      currRCol = [arrRelDef.foreign_key_constraint_on.column];
    } else {
      // check if this is already an existing manual relationship
      currTable = arrRelDef.manual_configuration.remote_table;
      currRCol = Object.values(arrRelDef.manual_configuration.column_mapping);
    }

    if (
      getTableName(currTable) === relTable &&
      sameRelCols(currRCol, relCols)
    ) {
      _isExistingArrRel = true;
      break;
    }
  }

  return _isExistingArrRel;
};

const suggestedRelationshipsRaw = (tableName, allSchemas, currentSchema) => {
  const objRels = [];
  const arrRels = [];

  const currentTableSchema = allSchemas.find(
    t => t.table_name === tableName && t.table_schema === currentSchema
  );

  const currentTableRelationships = currentTableSchema.relationships;

  const currentObjRels = currentTableRelationships.filter(
    r => r.rel_type === 'object'
  );
  const currentArrRels = currentTableRelationships.filter(
    r => r.rel_type === 'array'
  );

  currentTableSchema.foreign_key_constraints.forEach(fk_obj => {
    if (!fk_obj.is_ref_table_tracked) {
      return;
    }

    const lcol = Object.keys(fk_obj.column_mapping);

    if (!isExistingObjRel(currentObjRels, lcol)) {
      objRels.push({
        lTable: fk_obj.table_name,
        lSchema: fk_obj.table_schema,
        isObjRel: true,
        name: null,
        lcol: lcol,
        rcol: lcol.map(column => fk_obj.column_mapping[column]),
        rTable: fk_obj.ref_table,
        rSchema: fk_obj.ref_table_table_schema,
        isUnique: false,
      });
    }
  });

  currentTableSchema.opp_foreign_key_constraints.forEach(o_fk_obj => {
    if (!o_fk_obj.is_table_tracked) {
      return;
    }

    const rcol = Object.keys(o_fk_obj.column_mapping);
    const lcol = Object.values(o_fk_obj.column_mapping);
    const rTable = o_fk_obj.table_name;

    if (o_fk_obj.is_unique) {
      // if opp foreign key is also unique, make obj rel
      if (!isExistingObjRel(currentObjRels, lcol)) {
        objRels.push({
          lTable: o_fk_obj.ref_table,
          lSchema: o_fk_obj.ref_table_table_schema,
          name: null,
          rcol: rcol,
          lcol: rcol.map(column => o_fk_obj.column_mapping[column]),
          rTable: rTable,
          rSchema: o_fk_obj.table_schema,
          isObjRel: true,
          isUnique: true,
        });
      }
    } else {
      if (!isExistingArrRel(currentArrRels, rcol, o_fk_obj.table_name)) {
        arrRels.push({
          lTable: o_fk_obj.ref_table,
          lSchema: o_fk_obj.ref_table_table_schema,
          name: null,
          rcol: rcol,
          lcol: rcol.map(column => o_fk_obj.column_mapping[column]),
          rTable: rTable,
          rSchema: o_fk_obj.table_schema,
          isObjRel: false,
          isUnique: false,
        });
      }
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
