const fetch = require('node-fetch');
const throwError = require('../error');

const getObjRelationshipName = dep => {
  const relName = `${dep}By${dep[0].toUpperCase()}`;
  return dep.length === 1 ? relName + 'Id' : relName + dep.substring(1, dep.length) + 'Id';
};

const getArrayRelationshipName = (table, parent) => {
  const relName = `${table}sBy${parent[0].toUpperCase()}`;
  return parent.length === 1 ? `${relName}Id` : `${relName}${parent.substring(1, parent.length)}Id`;
};

const getArrayRelType = (table, child) => {
  const columnMapping = {};
  let numOfMappings = 0;
  table.columns.forEach(col => {
    if (col.name.indexOf(`__id`) === 0) {
      numOfMappings++;
      columnMapping[col.name] = `${table.name}_${col.name}`;
    }
  });
  if (numOfMappings === 1) {
    return {
      foreign_key_constraint_on: {
        table: child.name,
        column: columnMapping[Object.keys(columnMapping)[0]]
      }
    }
  } else {
    return {
      manual_configuration: {
        remote_table: child,
        column_mapping: columnMapping
      }
    }
  }
}

const getObjRelType = (table, dep) => {
  const relUsing = {};
  const columnMapping = {};
  let numOfMappings = 0;
  table.columns.forEach(col => {
    if (col.name.indexOf(`${dep.name}___id`) === 0) {
      numOfMappings++;
      columnMapping[col.name] = col.name.substring(col.name.indexOf('__id'), col.name.length);
    }
  });
  if (numOfMappings === 1) {
    return {
      foreign_key_constraint_on: Object.keys(columnMapping)[0]
    }
  } else {
    return {
      manual_configuration: {
        remote_table: dep.name,
        column_mapping: columnMapping
      }
    }
  }
}

const generateRelationships = tables => {
  const objectRelationships = [];
  const arrayRelationships = [];
  tables.forEach(table => {
    if (table.dependencies.length > 0) {
      table.dependencies.forEach(dep => {
        const objUsing = getObjRelType(table, tables.find((t) => t.name === dep));
        const arrUsing = getArrayRelType(tables.find((t) => t.name === dep), table);
        const newObjRel = {
          type: 'create_object_relationship',
          args: {
            table: table.name,
            name: dep,
            using: objUsing
          },
        };
        if (!objectRelationships.find((or) => {
          return (
            or.args.table === newObjRel.args.table &&
            or.args.name === newObjRel.args.name
          )
        })) {
          objectRelationships.push(newObjRel);
        }
        const newArrRel = {
          type: 'create_array_relationship',
          args: {
            table: dep,
            name: `${table.name}`,
            using: arrUsing
          },
        };
        if (!arrayRelationships.find((ar) => {
          return (
            ar.args.table === newArrRel.args.table &&
            ar.args.name === newArrRel.args.name
          );
        })) {
          arrayRelationships.push(newArrRel);
        }
      });
    }
  });
  return {
    objectRelationships,
    arrayRelationships,
  };
};

const createRelationships = async (tables, url, headers) => {
  const relationships = generateRelationships(tables);
  const bulkQuery = {
    type: 'bulk',
    args: [],
  };
  relationships.objectRelationships.forEach(or => bulkQuery.args.push(or));
  relationships.arrayRelationships.forEach(ar => bulkQuery.args.push(ar));
  const resp = await fetch(
    `${url}/v1/query`,
    {
      method: 'POST',
      body: JSON.stringify(bulkQuery),
      headers,
    }
  );
  if (resp.status !== 200) {
    const error = await resp.json();
    throwError(JSON.stringify(error, null, 2));
  }
};

module.exports = {
  createRelationships,
};
