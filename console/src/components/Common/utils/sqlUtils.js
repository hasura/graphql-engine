export const sqlEscapeText = text => {
  let _text = text;

  if (_text) {
    _text = _text.replace(/'/g, "\\'");
  }

  return `E'${_text}'`;
};

// detect DDL statements in SQL
export const checkSchemaModification = _sql => {
  let _isSchemaModification = false;

  const sqlStatements = _sql
    .toLowerCase()
    .split(';')
    .map(s => s.trim());

  sqlStatements.forEach(statement => {
    if (
      statement.startsWith('create ') ||
      statement.startsWith('alter ') ||
      statement.startsWith('drop ')
    ) {
      _isSchemaModification = true;
    }
  });

  return _isSchemaModification;
};
