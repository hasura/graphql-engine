export const sqlEscapeText = (rawText: string) => {
  let text = rawText;

  if (text) {
    text = text.replace(/'/g, "\\'");
  }

  return `E'${text}'`;
};

// detect DDL statements in SQL
export const checkSchemaModification = (sql: string) => {
  let isSchemaModification = false;

  const sqlStatements = sql
    .toLowerCase()
    .split(';')
    .map(s => s.trim());

  sqlStatements.forEach(statement => {
    if (
      statement.startsWith('create ') ||
      statement.startsWith('alter ') ||
      statement.startsWith('drop ')
    ) {
      isSchemaModification = true;
    }
  });

  return isSchemaModification;
};

export const getCheckConstraintBoolExp = (check: string) => {
  if (check) {
    return check.substring(7, check.length - 1);
  }

  return check;
};

/* queries */

export const getCreateCheckConstraintSql = (
  tableName: string,
  schemaName: string,
  constraintName: string,
  check: string
) => {
  return `alter table "${schemaName}"."${tableName}" add constraint "${constraintName}" check (${check})`;
};

export const getDropConstraintSql = (
  tableName: string,
  schemaName: string,
  constraintName: string
) => {
  return `alter table "${schemaName}"."${tableName}" drop constraint "${constraintName}"`;
};

export const getCreatePkSql = ({
  schemaName,
  tableName,
  selectedPkColumns,
  constraintName,
}: {
  schemaName: string;
  tableName: string;
  selectedPkColumns: string[];
  constraintName: string;
}) => {
  return `alter table "${schemaName}"."${tableName}"
    add constraint "${constraintName}" 
    primary key ( ${selectedPkColumns.map(pkc => `"${pkc}"`).join(', ')} );`;
};

export const getDropPkSql = ({
  schemaName,
  tableName,
  constraintName,
}: {
  schemaName: string;
  tableName: string;
  constraintName: string;
}) => {
  return `alter table "${schemaName}"."${tableName}" drop constraint "${constraintName}";`;
};

export const terminateSql = (sql: string) => {
  const sqlSanitised = sql.trim();
  return sqlSanitised[sqlSanitised.length - 1] !== ';'
    ? `${sqlSanitised};`
    : sqlSanitised;
};
