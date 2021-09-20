import { isRelationshipValid as isCitusRelValid } from '../../../../dataSources/services/citus/utils';

const sameRelCols = (currCols, existingCols) => {
  return currCols.sort().join(',') === existingCols.sort().join(',');
};

const checkEqual = (arr1, arr2) => {
  return (
    arr1 &&
    arr2 &&
    JSON.stringify([...arr1].sort()) === JSON.stringify([...arr2].sort())
  );
};

const isExistingObjRel = (currentObjRels, lcol, rcol = []) => {
  let _isExistingObjRel = false;

  for (let k = 0; k < currentObjRels.length; k++) {
    const objRelDef = currentObjRels[k].rel_def;

    if (objRelDef.foreign_key_constraint_on) {
      // check if this is already an existing fk relationship
      if (
        // TODO: update when multiCol fkey rels are allowed
        (lcol.length === 1 &&
          objRelDef.foreign_key_constraint_on === lcol[0]) ||
        (rcol.length === 1 &&
          objRelDef.foreign_key_constraint_on.column === rcol[0])
      ) {
        _isExistingObjRel = true;
        break;
      }
    } else {
      // check if this is already an existing manual relationship
      const objRelCols = Object.keys(
        objRelDef.manual_configuration.column_mapping
      );
      if (sameRelCols(objRelCols, lcol)) {
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

    if (currTable.name === relTable && sameRelCols(currRCol, relCols)) {
      _isExistingArrRel = true;
      break;
    }
  }

  return _isExistingArrRel;
};

const isRelationshipValid = (rel, allSchemas) => {
  const lTable = allSchemas.find(
    t => t.table_name === rel.lTable && t.table_schema === rel.lSchema
  );
  const rTable = allSchemas.find(
    t => t.table_name === rel.rTable && t.table_schema === rel.rSchema
  );

  /* valid relationship rules for citus */
  if (lTable?.citus_table_type && rTable?.citus_table_type) {
    return isCitusRelValid(rel, lTable, rTable);
  }

  return true;
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
        name: null,
        lcol: lcol,
        rcol: lcol.map(column => fk_obj.column_mapping[column]),
        rTable: fk_obj.ref_table,
        rSchema: fk_obj.ref_table_table_schema,
        isObjRel: true,
        isUnique: false,
        isPrimary: false,
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
    const rTableSchema = o_fk_obj.table_schema;
    const rTableObj = allSchemas.find(
      t => t.table_name === rTable && t.table_schema === rTableSchema
    );
    const pk = rTableObj?.primary_key?.columns;
    const is_primary_key = checkEqual(pk, rcol);
    let is_unique_key = false;

    if (
      rTableObj?.unique_constraints?.some(
        uk =>
          uk.constraint_name.includes('_key') && checkEqual(uk.columns, rcol)
      )
    ) {
      is_unique_key = true;
    }

    if (is_primary_key || is_unique_key) {
      // if opp foreign key is also unique or primary key, make obj rel
      if (!isExistingObjRel(currentObjRels, lcol, rcol)) {
        objRels.push({
          lTable: o_fk_obj.ref_table,
          lSchema: o_fk_obj.ref_table_table_schema,
          name: null,
          rcol: rcol,
          lcol: rcol.map(column => o_fk_obj.column_mapping[column]),
          rTable: rTable,
          rSchema: o_fk_obj.table_schema,
          isObjRel: true,
          isUnique: is_unique_key,
          isPrimary: is_primary_key,
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
          isPrimary: false,
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
    if (objRel !== null && isRelationshipValid(objRel, allSchemas)) {
      finalObjRel.push(objRel);
    }
    if (arrRel !== null && isRelationshipValid(arrRel, allSchemas)) {
      finalArrayRel.push(arrRel);
    }
  }

  return { objectRel: finalObjRel, arrayRel: finalArrayRel };
};

export default suggestedRelationshipsRaw;
