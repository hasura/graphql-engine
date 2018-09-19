const fetch = require('node-fetch');

const getObjRelationshipName = dep => {
  const relName = `${dep.substring(0, dep.length - 1)}By${dep[0].toUpperCase()}`;
  return dep.length === 0 ? relName + 'Id' : relName + dep.substring(1, dep.length) + 'Id';
};

const getArrayRelationshipName = (table, parent) => {
  const relName = `${table}By${parent[0].toUpperCase()}`;
  return parent.length === 1 ? `${relName}Id` : `${relName}${parent.substring(1, parent.length)}Id`;
};

const generateRelationships = tables => {
  const objectRelationships = [];
  const arrayRelationships = [];
  tables.forEach(table => {
    if (table.dependencies.length > 0) {
      table.dependencies.forEach(dep => {
        objectRelationships.push({
          type: 'create_object_relationship',
          args: {
            table: table.name,
            name: `${getObjRelationshipName(dep)}`,
            using: {
              foreign_key_constraint_on: `${dep.substring(0, dep.length - 1)}_id`,
            },
          },
        });
        arrayRelationships.push({
          type: 'create_array_relationship',
          args: {
            table: dep,
            name: `${getArrayRelationshipName(table.name, dep)}`,
            using: {
              foreign_key_constraint_on: {
                table: table.name,
                column: `${dep.substring(0, dep.length - 1)}_id`,
              },
            },
          },
        });
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
    console.log(JSON.stringify(error, null, 2));
    process.exit(1);
  }
};

module.exports = {
  createRelationships,
};
