interface SqlUtilsOptions {
  tableName: string;
  schemaName: string;
  constraintName: string;
  check?: string;
  selectedPkColumns?: string[];
}

export const sqlEscapeText = (text: string) => {
  let escapedText = text;

  if (escapedText) {
    escapedText = escapedText.replace(/'/g, "\\'");
  }

  return `E'${escapedText}'`;
};

// detect DDL statements in SQL
export const checkSchemaModification = (_sql: string) => {
  let isSchemaModification = false;

  const sqlStatements = _sql
    .toLowerCase()
    .split(';')
    .map(s => s.trim());

  sqlStatements.forEach((statement: string) => {
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
}: SqlUtilsOptions) => {
  // if no primary key columns provided, return empty query
  if (!selectedPkColumns || selectedPkColumns.length === 0) {
    return '';
  }

  return `alter table "${schemaName}"."${tableName}"
    add constraint "${constraintName}"
    primary key ( ${selectedPkColumns.map(pkc => `"${pkc}"`).join(', ')} );`;
};

export const getDropPkSql = ({
  schemaName,
  tableName,
  constraintName,
}: SqlUtilsOptions) => {
  return `alter table "${schemaName}"."${tableName}" drop constraint "${constraintName}";`;
};

export const terminateSql = (sql: string) => {
  const sqlTerminated = sql.trim();

  return sqlTerminated[sqlTerminated.length - 1] !== ';'
    ? `${sqlTerminated};`
    : sqlTerminated;
};
