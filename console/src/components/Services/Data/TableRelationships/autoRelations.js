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
    r => r.rel_type !== 'object'
  );
  for (let i = 0; i < allSchemas.length; i++) {
    const schema = allSchemas[i];
    const foreignKeyConstraints = schema.foreign_key_constraints;
    for (let j = 0; j < foreignKeyConstraints.length; j++) {
      const constraint = foreignKeyConstraints[j];
      if (constraint.table_name === tableName) {
        /* Object Relationships */
        const lcol = Object.keys(constraint.column_mapping);
        let isExistingObjRel = false;
        for (let k = 0; k < currentObjRels.length; k++) {
          if (currentObjRels[k].rel_def.foreign_key_constraint_on) {
            // check if this is already an existing relationship
            if (
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
            rcol: lcol.map(column => constraint.column_mapping[column]),
            rTable: constraint.ref_table,
          });
        }
      } else if (constraint.ref_table === tableName) {
        /* Array Relationships */
        const rcol = Object.keys(constraint.column_mapping);
        const rTable = constraint.table_name;
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
            currRCol = Object.values(
              relDef.manual_configuration.column_mapping
            );
          }

          if (
            currRCol.sort().join(',') === rcol.sort().join(',') &&
            getTableName(currTable) === constraint.table_name
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
            lcol: rcol.map(column => constraint.column_mapping[column]),
            rTable: rTable,
          });
        }
      }

      /* Self Referencing Array Relationships */
      if (
        constraint.ref_table === tableName &&
        constraint.table_name === tableName
      ) {
        const rcol = Object.keys(constraint.column_mapping);
        const rTable = constraint.table_name;
        let isExistingArrayRel = false;

        for (let k = 0; k < currentArrRels.length; k++) {
          // check if this is already an existing relationship
          if (
            currentArrRels[k].rel_def.foreign_key_constraint_on.column ===
              rcol &&
            currentArrRels[k].rel_def.foreign_key_constraint_on.table ===
              constraint.table_name
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
            lcol: rcol.map(column => constraint.column_mapping[column]),
            rTable: rTable,
          });
        }
      }
    }
  }

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
