const createSQLRegex = /create\s*(?:|or\s*replace)\s*(view|table|function)\s*(?:\s*if*\s*not\s*exists\s*)?((\"?\w+\"?)\.(\"?\w+\"?)|(\"?\w+\"?))/; // eslint-disable-line

const getSQLValue = value => {
  const quotedStringRegex = /^".*"$/;

  let sqlValue = value;
  if (!quotedStringRegex.test(value)) {
    sqlValue = value.toLowerCase();
  }

  return sqlValue.replace(/['"]+/g, '');
};

export const parseCreateSQL = sql => {
  const _objects = [];

  const regExp = createSQLRegex;

  const matches = sql.match(new RegExp(regExp, 'gmi'));
  if (matches) {
    matches.forEach(element => {
      const itemMatch = element.match(new RegExp(regExp, 'i'));

      if (itemMatch && itemMatch.length === 6) {
        const _object = {};

        const type = itemMatch[1];

        // If group 5 is undefined, use group 3 and 4 for schema and table respectively
        // If group 5 is present, use group 5 for table name using public schema.
        let name;
        let schema;
        if (itemMatch[5]) {
          name = itemMatch[5];
          schema = 'public';
        } else {
          name = itemMatch[4];
          schema = itemMatch[3];
        }

        _object.type = type.toLowerCase();
        _object.name = getSQLValue(name);
        _object.schema = getSQLValue(schema);

        _objects.push(_object);
      }
    });
  }

  return _objects;
};

export const getStatementTimeoutSql = statementTimeoutInSecs => {
  return `SET LOCAL statement_timeout = ${statementTimeoutInSecs * 1000}`;
};
